Game of Life
============

This is a project where I closely follow the code described by *Andrew Rademacher*
in his blog post [Haskell Optimization and the Game of Life](http://blog.headcrab.info/haskell-optimization-and-the-game-of-life/).
The idea of the project was not only to implement the Game of Life but also to
show how it can be optimized, in what ways we can identify the problematic
chunks of code, etc.

As a result of this project I have somewhat deviated from the code provided in
the article and made a few optimizations of my own. I did not include the REPA
change provided in the article. Therefore my final code is not as fast as the
one built in the blog post.

Here is a brief summary of what is going on during the project development:

### Rules of the game ###

The game of life is a simulation of cellular automaton. Each cell is arranged in a 
2-D grid and has 8 neighboring cells. The life cycle of a cell is dictated by a
simple set of rules.

* If a cell is alive and has fewer than 2 neighbors, it dies as if by loneliness.
* If a cell is alive and has more than 3 neighbors, it dies as if by starvation due to over-crowding.
* If a cell is alive and has 2 or 3 neighbors it remains alive.
* If a cell is dead and has exactly 3 neighbors it spawns, as if by reproduction.

A further description of the game of life can be found at [Wikipedia](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

### Representation using Gloss ###

![Game of life representation using Gloss](http://ksaveljev.github.io/game-of-life.png)

### Initial version ###

The decision is made to use Vector instead of List to keep track of the board
state. Each cell is represented as an Int instead of Boolean (which could
indicate if the cell is alive or dead).

It is shown that with this version it might take something like a minute to run
100 generations for a 900x900 board.

### Profiling ###

In order to identify the problem(s) we need to have a look at performance report
from GHC. We use the `-rtsopts` compilation flag and then run our executable
with `+RTS -s` arguments in order to the statistics summary.

It shows us the running time, GC time and allocation rate. In order to get more
detailed information we need to look at the cost centre output from GHC. And in
order to do that we need to add `library-profiling: True` and
`executable-profiling: True` to the cabal sandox config file and reinstall all
the dependencies. We need to compile our executable with `-fprof-auto` flag, so
that all the functions which are not marked as inlined will be marked as a cost
centre. And finally when we run our executable we use `+RTS -p` arguments.

### Huge allocation rate ###

The first thing that is spotted is the allocation rate in `nextCell.nc` which is
fixed by switching from List.sum to simply summing up all the arguments. This
change helps us to bring down the allocation rate (cut in half).

### Strictness ###

It is then proposed to add strictness to most of the fields we use in Life. This
however does not bring immediate benefit. Although some annotations do help.

### Unboxing data ###

We switch to unboxed vector in order to increase performance. It is said that
with boxed version we always access RAM (and not CPU register or cache) and it
takes a bit of clock cycles. Unboxed vector brings down Garbage Collection to
about 1% of time spent (before we spent 25-30% in GC). The running time is must
faster now.

### Using REPA ###

Next step in the blog article (which I haven't done) was to use REPA to enable
parallel computing using vector as underlying data structure. This also helps to
bring down the runtime of the program.

### Inlining functions ###

Finally if we look at the cost centre we can see that some functions are called
so many times that the overhead of calling these functions must be enormous. We
add the INLINE pragma to decrease the running time even more.

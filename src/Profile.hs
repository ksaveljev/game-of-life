{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Text ()
import System.Random (newStdGen, StdGen)
import System.Console.CmdArgs (cmdArgs, Typeable, Data, (&=), help)
import qualified Data.Text.Format as TF

import Life

data Profile = Profile { width_ :: Int
                       , height_ :: Int
                       , generations_ :: Int
                       } deriving (Show, Typeable, Data)

argsProfile :: Profile
argsProfile = Profile { width_       = 100 &= help "The number of cells across the game board."
                      , height_      = 100 &= help "The number of cells tall."
                      , generations_ = 200 &= help "The number of generations to calculate."
                      }

makeFirstGen :: Profile -> StdGen -> Generation
makeFirstGen (Profile w h _) seed = Generation w h 1 1 (randomBoard w h seed)

simulate :: Int -> Generation -> Int
simulate 0 gen = countAlive gen
simulate i gen = simulate (i - 1) (nextGeneration gen)

main :: IO ()
main = do
    profile <- cmdArgs argsProfile
    seed <- newStdGen
    let firstGen = makeFirstGen profile seed
        finalAlive = simulate (generations_ profile) firstGen
    TF.print "Final Alive: {}\n" $ TF.Only finalAlive

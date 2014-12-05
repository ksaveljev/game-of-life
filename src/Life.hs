{-# LANGUAGE BangPatterns #-}

module Life ( Board
            , Generation(..)
            , randomBoard
            , nextGeneration
            , getCoords
            , countAlive
            ) where

import System.Random
import qualified Data.Vector.Unboxed as U

-- Way too many object created if we use boxed vector
--type Board = V.Vector Int
type Board = U.Vector Int
type Width = Int
type Height = Int

data Generation = Generation { width     :: !Int
                             , height    :: !Int
                             , cellWidth :: !Float
                             , genPerSec :: !Int
                             , board     :: !Board
                             }

randomBoard :: Width -> Height -> StdGen -> Board
randomBoard w h = U.take (w * h) . U.unfoldr (Just . randomR (0, 1))

nextGeneration :: Generation -> Generation
nextGeneration gen@(Generation w h cw gps brd) =
    Generation w h cw gps (U.imap (nextCell gen) brd)

{-# INLINE getCoords #-}
getCoords :: Generation -> Int -> (Int, Int)
getCoords (Generation w _ _ _ _) !idx = idx `quotRem` w

{-# INLINE fromCoords #-}
fromCoords :: Generation -> (Int, Int) -> Int
fromCoords (Generation w _ _ _ _) (!x, !y) = x + (y * w)

countAlive :: Generation -> Int
countAlive (Generation _ _ _ _ brd) = U.sum brd

{-# INLINE nextCell #-}
nextCell :: Generation -> Int -> Int -> Int
nextCell gen@(Generation _ _ _ _ brd) !idx !state
  | nc < 2 || nc > 3 = 0
  | nc == 3 = 1
  | otherwise = state
  where
    (!x, !y) = getCoords gen idx
    {-
    -- this way it creates a huge allocation rate
    nc = L.sum [ gn neg neg, gn zro neg, gn pos neg
               , gn neg zro, 0         , gn pos zro
               , gn neg pos, gn zro pos, gn pos pos]
    -}
    !nc = gn neg neg + gn zro neg + gn pos neg
        + gn neg zro + gn zro zro + gn pos zro
        + gn neg pos + gn zro pos + gn pos pos
    gn !mx !my
      | midx < 0 = 0
      | midx >= U.length brd = 0
      | otherwise  = brd U.! midx
      where
        !midx = fromCoords gen (x + mx, y + my)
    !neg = -1
    !pos = 1
    !zro = 0

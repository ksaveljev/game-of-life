module Life ( Board
            , Generation(..)
            , randomBoard
            , nextGeneration
            , getCoords
            , countAlive
            ) where

import System.Random
import qualified Data.Vector as V

type Board = V.Vector Int
type Width = Int
type Height = Int

data Generation = Generation { width     :: Int
                             , height    :: Int
                             , cellWidth :: Float
                             , genPerSec :: Int
                             , board     :: Board
                             }

randomBoard :: Width -> Height -> StdGen -> Board
randomBoard w h = V.take (w * h) . V.unfoldr (Just . randomR (0, 1))

nextGeneration :: Generation -> Generation
nextGeneration gen@(Generation w h cw gps brd) =
    Generation w h cw gps (V.imap (nextCell gen) brd)

getCoords :: Generation -> Int -> (Int, Int)
getCoords gen idx = (x, y)
  where
    x = idx `mod` width gen
    y = idx `div` width gen

fromCoords :: Generation -> (Int, Int) -> Int
fromCoords gen (x, y) = x + (y * width gen)

countAlive :: Generation -> Int
countAlive (Generation _ _ _ _ brd) = V.sum brd

nextCell :: Generation -> Int -> Int -> Int
nextCell gen idx state
  | nc < 2 || nc > 3 = 0
  | nc == 3 = 1
  | otherwise = state
  where
    (x, y) = getCoords gen idx
    {-
    -- this way it creates a huge allocation rate
    nc = L.sum [ gn neg neg, gn zro neg, gn pos neg
               , gn neg zro, 0         , gn pos zro
               , gn neg pos, gn zro pos, gn pos pos]
    -}
    nc = gn neg neg + gn zro neg + gn pos neg
       + gn neg zro + gn zro zro + gn pos zro
       + gn neg pos + gn zro pos + gn pos pos
    gn mx my
      | midx < 0 = 0
      | midx >= V.length (board gen) = 0
      | otherwise  = board gen V.! midx
      where
        midx = fromCoords gen (x + mx, y + my)
    neg = -1
    pos = 1
    zro = 0

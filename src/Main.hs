{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Text ()
import System.Random (newStdGen, StdGen)
import System.Console.CmdArgs (cmdArgs, (&=), help, Typeable, Data)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

import Life

data Life = Life { width_     :: Int
                 , height_    :: Int
                 , cellWidth_ :: Int
                 , genPerSec_ :: Int
                 } deriving (Show, Typeable, Data)

argsLife :: Life
argsLife = Life { width_     = 50 &= help "The number of cells across the game board."
                , height_    = 50 &= help "The number of cells tall."
                , cellWidth_ = 10 &= help "The number of pixels across a single cell."
                , genPerSec_ = 1  &= help "The number of generations per second."
                }

main :: IO ()
main = do
    life <- cmdArgs argsLife
    seed <- newStdGen
    let firstGen = makeFirstGen life seed
        displayWidth = cellWidth_ life * width_ life
        displayHeight = cellWidth_ life * height_ life
    simulate (InWindow "Game of Life" (displayWidth, displayHeight) (10, 10))
      white (genPerSec firstGen) firstGen pictureGeneration makeNextGen
    undefined

makeFirstGen :: Life -> StdGen -> Generation
makeFirstGen (Life w h cw gps) seed =
    Generation w h (fromIntegral cw :: Float) gps (randomBoard w h seed)

makeNextGen :: ViewPort -> Float -> Generation -> Generation
makeNextGen _ _ = nextGeneration

pictureGeneration :: Generation -> Picture
pictureGeneration gen = Translate tx ty
                      $ Scale (cellWidth gen) (cellWidth gen)
                      $ Pictures (V.toList (V.imap (pictureCell gen) (G.convert (board gen) :: V.Vector Int)))
  where
    tx = negate ((cellWidth gen * w) / 2)
    ty = negate ((cellWidth gen * h) / 2)
    w = fromIntegral (width gen) :: Float
    h = fromIntegral (height gen) :: Float

pictureCell :: Generation -> Int -> Int -> Picture
pictureCell gen idx state = Translate (fromIntegral x :: Float) (fromIntegral y :: Float)
                          $ Color (stateColor state)
                          $ Polygon [(0.1, 0.1), (0.1, 0.9), (0.9, 0.9), (0.9, 0.1)]
  where
    (x, y) = getCoords gen idx
    stateColor 0 = makeColor 0.9 0.9 0.9 1
    stateColor 1 = makeColor 0 0 0 1
    stateColor _ = makeColor 0 0 0 0

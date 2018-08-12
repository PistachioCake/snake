{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( Unit (Unit), Game (..), 
      Vel, up, down, left, right, render,
      squareSize, height, width, boundX, boundY,
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Random

newtype Unit = Unit (Int, Int) deriving (Eq, Show)
type Vel = (Int, Int)
data Game = Begin | Game {_gen :: StdGen, _add :: Int, _vel :: Vel, _food :: Unit, _snake :: [Unit]} | Lose {_score :: Int}

up, down, left, right :: Vel
up = (0, 1)
down = (0, -1)
left = (-1, 0)
right = (1, 0)

squareSize, drawSize :: Int
squareSize = 20
drawSize = 18

height, width :: Int
height = 30
width = 20

boundX, boundY :: (Int, Int)
boundX = (-height `div` 2, height `div` 2 - 1)
boundY = (-width `div` 2, width `div` 2 - 1)

class Renderable a where
    render :: a -> Picture

instance Renderable Unit where
    render (Unit (x, y)) = translate (fi $ squareSize `div` 2 + squareSize * x) (fi $ squareSize `div` 2 + squareSize * y) $ rectangleSolid (fi drawSize) (fi drawSize)
        where fi = fromIntegral

instance Renderable Game where
    render Begin = translate (-150) 0 $ scale 0.15 0.15 $ color white $ text "Press space to begin."
    render (Game {_food = food, _snake = snake}) = pictures $ (color red $ render food):(map (color green . render) snake)
    render (Lose {_score = score}) = translate (-150) 0 $ color white $ pictures [
        translate 0 50 $ scale 0.3 0.3 $ text "Game Over!",
        scale 0.2 0.2 $ text $ "Your score was " ++ show score ++ ".", 
        translate 0 (-30) $ scale 0.15 0.15 $ text "Press R to play again.", 
        translate 0 (-50) $ scale 0.15 0.15 $ text "Press Q to quit."]
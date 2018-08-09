module Lib
    ( Unit (Unit), Game (Game), 
      Vel, up, down, left, right, render
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

newtype Unit = Unit (Int, Int)
type Vel = (Int, Int)
data Game = Game {velocity :: Vel, snake :: [Unit]}

up, down, left, right :: Vel
up = (0, 1)
down = (0, -1)
left = (-1, 0)
right = (1, 0)

class Renderable a where
    render :: a -> Picture

instance Renderable Unit where
    render (Unit (x, y)) = color green $ translate (10 + 20 * fromIntegral x) (10 + 20 * fromIntegral y) $ rectangleSolid 18 18

instance Renderable Game where
    render (Game _ snake) = pictures $ map render snake
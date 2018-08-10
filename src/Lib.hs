{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( Unit (Unit), Game (Begin, Game), 
      Vel, up, down, left, right, render
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Random

newtype Unit = Unit (Int, Int) deriving (Eq, Show)
type Vel = (Int, Int)
data Game = Begin | Game {gen :: StdGen, add :: Int, velocity :: Vel, food :: Unit, snake :: [Unit]}

up, down, left, right :: Vel
up = (0, 1)
down = (0, -1)
left = (-1, 0)
right = (1, 0)

class Renderable a where
    render :: a -> Picture

instance Renderable Unit where
    render (Unit (x, y)) = translate (10 + 20 * fromIntegral x) (10 + 20 * fromIntegral y) $ rectangleSolid 18 18

instance Renderable Game where
    render Begin = blank
    render (Game _ _ _ food snake) = pictures $ (color red $ render food):(map (color green . render) snake)
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( Unit (Unit), Game (..), 
      Vel, up, down, left, right, render,
      squareSize, height, width, boundX, boundY,
    ) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import System.Random

-- | A datatype for representing anything that is on the board.
newtype Unit = Unit (Int, Int) deriving (Eq, Show)
-- | A type synonym to specify velocities.
type Vel = (Int, Int)
-- | The actual type of the game.
data Game = Begin -- ^ The beginning, representing the intro screen.
          | Game  -- ^ The actual, running game.
            { _gen :: StdGen     -- ^ The generator for random numbers.
            , _add :: Int        -- ^ The length of the snake yet to add.
            , _vel :: Vel        -- ^ The direction the head of the snake is going in.
            , _food :: Unit      -- ^ The position of the food.
            , _snake :: [Unit]   -- ^ The position of all the units of the snake.
            } 
          | Lose {_score :: Int}

-- | Constants that are used to manipulate the direction. 
up, down, left, right :: Vel
up = (0, 1)
down = (0, -1)
left = (-1, 0)
right = (1, 0)

-- | Constants that manipulate the images rendered.
squareSize, drawSize :: Int
-- | The size of each cell.
squareSize = 20
-- | The size of the colored part of each cell.
drawSize = 18

-- | The dimensions of the drawing.
height, width :: Int
height = 30
width = 20

-- | The boundaries of the board.
boundX, boundY :: (Int, Int)
boundX = (-height `div` 2, height `div` 2 - 1)
boundY = (-width `div` 2, width `div` 2 - 1)

-- | A typeclass for rendering data to pictures.
class Renderable a where
    render :: a -> Picture -- ^ The polymorphic render funciton.

instance Renderable Unit where
    render (Unit (x, y)) = translate (fi $ squareSize `div` 2 + squareSize * x) (fi $ squareSize `div` 2 + squareSize * y) $ rectangleSolid (fi drawSize) (fi drawSize)
        where fi = fromIntegral

instance Renderable Game where
    -- | Display "Press space to begin." before the game beigns.
    render Begin = translate (-150) 0 $ scale 0.15 0.15 $ color white $ text "Press space to begin."
    -- | Display the food and snake when the game is running.
    render (Game {_food = food, _snake = snake}) = pictures $ (color red $ render food):(map (color green . render) snake)
    -- | Render some text when the game ends, including the score.
    render (Lose {_score = score}) = translate (-150) 0 $ color white $ pictures [
        translate 0 50 $ scale 0.3 0.3 $ text "Game Over!",
        scale 0.2 0.2 $ text $ "Your score was " ++ show score ++ ".", 
        translate 0 (-30) $ scale 0.15 0.15 $ text "Press R to play again.", 
        translate 0 (-60) $ scale 0.15 0.15 $ text "Press Q to quit."]
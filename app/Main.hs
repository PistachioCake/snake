module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit (exitSuccess)
import Lib

-- | A helper function to easily build the original gamestate
(<@>) :: (Applicative f) => f (a -> b) -> a -> f b
f <@> a = f <*> pure a
infixl 4 <@>

main :: IO ()
main = playIO window bkg fps initialState renderIO handleEvents update
    where 
        window = InWindow "Snake" (height * squareSize, width * squareSize) (10, 10)
        bkg = black
        fps = 10
        renderIO :: Game -> IO Picture
        renderIO g = return $ render g

-- | The initial state of the game - will be updated to allow play
initialState :: Game
initialState = Begin

-- | The update function, which steps the game to the next iteration.
--   It uses the helper functions defined below.
update :: Float -> Game -> IO Game
update _ Begin = return Begin
update _ (Lose {_score = score}) = return $ Lose score
update _ game = 
    if hasLost game 
    then return $ Lose $ (length $ _snake game) `div` 3 - 1
    else return . moveSnake . eatFood $ game

-- | Moves the snake by adding a new unit to move the head, and
--   removing a unit for the tail if appropriate. 
moveSnake :: Game -> Game
moveSnake game@(Game {_add = add, _vel = (velx, vely), _snake = snake}) = 
    let Unit (frontx, fronty) = head snake
    in game {_add = max 0 (add - 1), _snake = Unit (velx + frontx, vely + fronty):(if add > 0 then snake else init snake)}

-- | Allows the snake to eat food if its head is directly on the food
eatFood :: Game -> Game
eatFood game@(Game {_add = add, _food = food, _snake = (front:_)}) = 
    if front == food then mkNextFood $ game {_add = add + 3} else game

-- | Creates a new food unit using the StdGen in the Game. Shoule
--   only be called when the snake has eaten the previous food. 
mkNextFood :: Game -> Game
mkNextFood game@(Game {_gen = gen, _snake = snake}) = 
    let (x, gen') = randomR boundX gen
        (y, gen'') = randomR boundY gen'
        food = Unit (x, y)
    in if food `elem` snake then mkNextFood $ game {_gen = gen''} else game {_gen = gen'', _food = food}

-- | Checks if the game is lost, by checking whether the 
--   snake has hit the wall or its tail. 
hasLost :: Game -> Bool
hasLost game@(Game {_snake = (front:tail)}) = wallCollision || tailCollision
    where
        wallCollision = 
            let notInRange (l, h) a = a < l || h < a
                (Unit (x, y)) = front
            in notInRange boundX x || notInRange boundY y
        tailCollision = front `elem` tail

-- | Handle keypresses by modifying the game state accordingly. 
handleEvents :: Event -> Game -> IO Game
-- Change the direction of the snake.
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) game@(Game { }) = return game {_vel = up}
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game@(Game { }) = return game {_vel = down}
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game@(Game { }) = return game {_vel = left}
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game@(Game { }) = return game {_vel = right}
-- Begin the game.
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) Begin = Game <$> getStdGen <@> 0 <@> up <@> Unit (0, 0) <@> [Unit (0, 0)]
-- Reset the game.
handleEvents (EventKey (Char 'r') Down _ _) (Lose s) = Game <$> getStdGen <@> 0 <@> up <@> Unit (0, 0) <@> [Unit (0, 0)]
-- Quit the game.
handleEvents (EventKey (Char 'q') Down _ _) _ = exitSuccess
-- If all other patterns fail, do nothing. 
handleEvents _ game = return game

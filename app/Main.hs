module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib


main :: IO ()
main = play window bkg fps initialState render handleEvents (const update)
    where 
        window = InWindow "Test" (600, 400) (10, 10)
        bkg = black
        fps = 2

initialState :: Game
initialState = Game up [Unit (0, 0)]

update :: Game -> Game
update (Game vel@(velx, vely) units) = 
    let (Unit front) = head units
        end = init units
    in Game vel $ Unit (velx + fst front, vely + snd front):end

handleEvents :: Event -> Game -> Game
handleEvents (EventKey key Down _ _) (Game vel snake) = Game
    (case key of
        SpecialKey KeyUp -> up
        SpecialKey KeyDown -> down
        SpecialKey KeyLeft -> left
        SpecialKey KeyRight -> right
        _ -> vel)
    snake
handleEvents _ game = game
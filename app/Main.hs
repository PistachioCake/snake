module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Lib


main :: IO ()
main = playIO window bkg fps initialState renderIO handleEventsIO updateIO
    where 
        window = InWindow "Test" (600, 400) (10, 10)
        bkg = black
        fps = 2
        renderIO :: Game -> IO Picture
        renderIO g = return $ render g
        handleEventsIO :: Event -> Game -> IO Game
        handleEventsIO e g = return $ handleEvents e g
        updateIO :: Float -> Game -> IO Game
        updateIO f g = return $ update f g


initialState :: Game
initialState = Game up [Unit (0, 0)]

update :: Float -> Game -> Game
update _ (Game vel@(velx, vely) units) = 
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
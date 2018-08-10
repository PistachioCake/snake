module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Lib

(<@>) :: (Applicative f) => f (a -> b) -> a -> f b
f <@> a = f <*> pure a
infixl 4 <@>

main :: IO ()
main = playIO window bkg fps initialState renderIO handleEventsIO update
    where 
        window = InWindow "Test" (600, 400) (10, 10)
        bkg = black
        fps = 5
        renderIO :: Game -> IO Picture
        renderIO g = return $ render g
        handleEventsIO :: Event -> Game -> IO Game
        handleEventsIO e g = return $ handleEvents e g


initialState :: Game
initialState = Begin

update :: Float -> Game -> IO Game
update _ Begin = Game <$> getStdGen <@> 0 <@> up <@> Unit (0, 0) <@> [Unit (0, 0)]
update _ (Game gen oldadd vel@(velx, vely) food snake) = 
    let front = head snake
        (Unit frontPos) = front
        add = oldadd + if front == food then 3 else 0
        end = drop 1 $ if add > 0 then init snake else snake
    in 
        if front == food then
            let (x, gen') = randomR (-10, 10) gen
                (y, gen'') = randomR (-15, 15) gen'
            in return $ Game gen'' (oldadd + 2) vel (Unit (x, y)) $ Unit (velx + fst frontPos, vely + snd frontPos):snake
        else
            return $ Game gen (max 0 (oldadd - 1)) vel food $ Unit (velx + fst frontPos, vely + snd frontPos):(if oldadd > 0 then snake else init snake)

handleEvents :: Event -> Game -> Game
handleEvents (EventKey key Down _ _) (Game gen add vel food snake) = 
    let newvel = case key of
            SpecialKey KeyUp -> up
            SpecialKey KeyDown -> down
            SpecialKey KeyLeft -> left
            SpecialKey KeyRight -> right
            _ -> vel
    in Game gen add newvel food snake
handleEvents _ game = game
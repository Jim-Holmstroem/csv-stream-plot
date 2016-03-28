{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Data.Monoid ((<>), mconcat, mempty)
import Graphics.Gloss.Interface.IO.Game

import Text.Read
import Data.List.Split


import Debug.Trace


-- write down libraries (and versions) used
-- gloss, split

-- use TVar?
-- Gloss.Interface.IO.Simulate instead?

-- TODO why is there extra newlines after each new line from /dev/ttyUSB0 ? it wasnt there yesterday..

type Readings = [Float]

readData :: MVar Readings -> IO ()
readData readings = do
    input <- getContents
    let rows = tail $ lines input

    mapM_ handle rows
        where
            handle row = save v
                where v = mapM readMaybe $ splitOn "," row
            save Nothing = return ()
            save (Just (value:_)) = putStrLn (show value) >> modifyMVar_ readings (return . (value/128:))



draw :: MVar Readings -> IO Picture
draw readings = do
    readReadings <- readMVar readings

    return $ graph readReadings <> start readReadings
        where graph readReadings = color white $ line $ zip [-400,-397..] (take 512 readReadings)
              start [] = mempty
              start xv@(x:_) = translate (-400) x $ color red $ circle lastVariance
                where lastVariance = sqrt $ (sum $ map (\x->(x-lastMean)^2) lastReadings) / (max 1 lastLength)
                      lastMean = (sum lastReadings) / lastLength
                      lastLength = fromIntegral $ length lastReadings
                      lastReadings = take 8 xv




eventHandler :: Event -> world -> IO world
eventHandler _ w = return w

updateHandler :: Float -> world -> IO world
updateHandler dt w = return w

main :: IO ()
main = do
    readings <- newMVar []

    void $ forkIO $ readData readings

    playIO
        (InWindow "StreamPlotter" (1, 1) (512, 512)) -- display mode
        black -- bgcolor
        60 -- fps
        readings -- :: world, initial state
        draw -- :: (world -> IO Picture), view
        eventHandler -- :: (Event -> world -> IO world), eventHandler
        updateHandler -- :: (Float -> world -> IO world), update

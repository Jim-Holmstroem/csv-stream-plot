{-# LANGUAGE OverloadedStrings #-}

module Core ( runPlot
            , module Plot
            , module Expression
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Data.Monoid ((<>), mconcat, mempty)

import Text.Read
import System.IO
import System.Environment

import Data.List.Split

import Graphics.Gloss.Interface.IO.Game

import Plot
import Expression

-- write down libraries (and versions) used
-- gloss, split

-- TODO why is there extra newlines after each new line from /dev/ttyUSB0 ? "it wasnt there yesterday"..
-- TODO make it at least drop the row if it changes in length

-- TODO everything but a EDSL statement like ``main = plot $ DefaultPlot $ map x [0..5]``
-- the rest is moved out into the core


type Readings = [[Double]]

readData :: (Plot p) => String -> MVar p -> IO ()
readData filename plot = do
    --input <- getContents

    handle <- openFile filename ReadMode
    input <- hGetContents handle

    let rows = tail $ lines input

    mapM_ process rows
    hClose handle
        where
            process row = save $ mapM (readMaybe :: String -> Maybe Double) $ splitOn "," row
            save Nothing = return ()
            save (Just []) = return ()
            save (Just values) = do  -- TODO is modifyMVar the right thing todo here?
                modifyMVar_ plot $ \p -> return $ append values p
            --save (Just values) = do
            --    modifyMVar_ readings $ \readings-> return $ zipWith (:) values $ enforceDimension values readings
            --        where
            --            enforceDimension left [] = map (const []) left
            --            enforceDimension left right = right


draw :: (Plot p) => MVar p -> IO Picture
draw plot = do
    readPlot <- readMVar plot -- TODO better name than readPlot/plot

    return $ render readPlot
--    return $ pictures $ renderVariable <$> readReadings
--        where renderVariable readReadingsVariable = graph readReadingsVariable <> start readReadingsVariable
--              graph readReadingsVariable = color white $ line $ zip [-400,-395..] (take 512 readReadingsVariable)
--              start [] = mempty
--              start xv@(x:_) = translate (-400) x $ color red $ circle lastVariance
--                where lastVariance = sqrt $ (sum $ map (\x->(x-lastMean)^2) lastReadings) / (max 1 lastLength)
--                      lastMean = (sum lastReadings) / lastLength
--                      lastLength = fromIntegral $ length lastReadings
--                      lastReadings = take 8 xv


eventHandler :: Event -> world -> IO world
eventHandler _ w = return w


updateHandler :: Float -> world -> IO world
updateHandler dt w = return w



-- TODO Fix the bug (see below message)
-- csv-stream-plot: /dev/ttyUSB0: hGetContents: invalid argument (invalid byte sequence)

runPlot :: (Plot p) => p -> IO ()
runPlot p = do
    filename <- head <$> getArgs
    plot <- newMVar p

    void $ forkIO $ readData filename plot -- TODO better name for readData

    playIO
        (InWindow "StreamPlotter" (1, 1) (512, 512)) -- display mode
        black -- bgcolor
        60 -- fps
        plot -- :: world, initial state
        draw -- :: (world -> IO Picture), view
        eventHandler -- :: (Event -> world -> IO world), eventHandler
        updateHandler -- :: (Float -> world -> IO world), update

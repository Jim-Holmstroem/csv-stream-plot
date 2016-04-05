{-# LANGUAGE OverloadedStrings #-}

module Core ( runPlot
            , module Plot
            , module Expression
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Data.Monoid ((<>), mconcat, mempty)

import Text.Read hiding (get)
import System.IO
import System.Environment

import Data.List.Split

import Graphics.UI.GLUT (Size(..), get, windowSize)
import Graphics.Gloss.Interface.IO.Game

import Plot
import Expression

import Debug.Trace

-- write down libraries (and versions) used
-- gloss, split

-- TODO why is there extra newlines after each new line from /dev/ttyUSB0 ? "it wasnt there yesterday"..
-- TODO make it at least drop the row if it changes in length

-- TODO everything but a EDSL statement like ``main = plot $ DefaultPlot $ map x [0..5]``
-- the rest is moved out into the core


type Readings = [[Double]]


collectData :: String -> MVar Plot -> IO ()
collectData filename plot = do
    withFile filename ReadMode $ \handle -> do
        hSetEncoding handle char8 -- FIX Fixes (probably) the bug: hGetContents: invalid argument (invalid byte sequence). char8 allows to read and write all byte sequences. The garbage it potentially reads in the start will be thrown away anyway it just needs to be readable.
        rows <- tail . lines <$> hGetContents handle

        mapM_ process rows
            where
                process row = save $ mapM (readMaybe :: String -> Maybe Double) $ splitOn "," row
                save Nothing = return ()
                save (Just []) = return ()
                save (Just values) = do
                    modifyMVar_ plot $ return . (append values)


draw :: MVar Plot -> IO Picture
draw plotState = do
    plot <- readMVar plotState
    (Size w h) <- get windowSize
    let scaleNormalized = scale ((realToFrac w)/2) ((realToFrac h)/2)
    return $ scaleNormalized $ render plot

eventHandler :: Event -> world -> IO world
eventHandler = const return

updateHandler :: Float -> world -> IO world
updateHandler = const return

runPlot :: String -> Plot -> IO ()
runPlot name p = do
    filename <- head <$> getArgs
    plot <- newMVar p

    void $ forkIO $ collectData filename plot

    playIO
        (InWindow name (1, 1) (512, 512))
        black
        60
        plot
        draw
        eventHandler
        updateHandler

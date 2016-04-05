{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Plot ( Plot(..)
            , Plot_(..)
            , EmptyPlot(..)
            , OrdinaryPlot(..)
            , ordinaryPlot
            , VerticalSplit(..)
            , verticalSplit
            , render
            , append
) where

import Data.Monoid ((<>), mempty, mconcat)

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Expression

-- TODO use those derivative of a list things that is used in xmonad for window switching
-- TogglePlot Key [Plot], toggle between plots on keys
-- KeyPlot (Map Key Plot), switch between plots on keys

-- Ontop [Plot] (render plots on top of each other (there is issues with this like sharing exprs and similar
-- Plot should only have the logic to render, not the logic to have data saved (this so that it can be modularized and the data could be shared among different plots (thing about how OnTop [Plot] should work.

-- TODO think long and hard about having Plot have the class Plot_ thing and what it returns etc..

-- TODO is there a way to clip so that a plot never renders outside of it's [-1,1]-square?

class Plot_ p where
    render :: p -> Picture
    append :: [Double] -> p -> Plot  -- TODO FIXME is returning Plot here weird?
    -- TODO should the arguments be the other way around?
data Plot = forall p. (Plot_ p) => Plot p
instance Plot_ Plot where
    render (Plot p) = render p
    append values (Plot p) = append values p

data EmptyPlot = EmptyPlot
    deriving (Show, Eq)
instance Plot_ EmptyPlot where
    render = const blank
    append _ p = emptyPlot
emptyPlot :: Plot
emptyPlot = Plot EmptyPlot


data OrdinaryPlot = OrdinaryPlot [Expression] [[Double]]
instance Plot_ OrdinaryPlot where
    render (OrdinaryPlot _ evaluations) = zeroLine <> (pictures $ renderEvaluation <$> evaluations)
        where renderEvaluation = graph
              zeroLine = color (greyN 0.3) $ line [(-1, 0), (1, 0)]
              nSteps = 512 :: Int
              dt = 2/(realToFrac nSteps)
              graph evaluation = color white $ line $ zip [-1.0, (-1.0+dt)..] (take nSteps $ map ((/180).realToFrac) evaluation)
    append values (OrdinaryPlot exprs evaluations) = Plot $ OrdinaryPlot exprs $ zipWith (:) evaluation evaluations
        where
            evaluation = map (`eval` values) exprs

ordinaryPlot :: [Expression] -> Plot
ordinaryPlot exprs = Plot $ OrdinaryPlot exprs $ map (const []) exprs


-- TODO deriving Show?
data VerticalSplit = VerticalSplit [Plot]
instance Plot_ VerticalSplit where
    append values (VerticalSplit plots) = verticalSplit $ map (append values) plots
    render (VerticalSplit []) = blank
    render (VerticalSplit plots) = splits <> (pictures $ zipWith ($) translateSlices (map (scaleSlice.render) plots))
        where nSlices = length plots
              sliceWidth = 2.0/realToFrac nSlices
              scaleSlice = scale (sliceWidth/2) 1.0
              translateSlices = map (`translate` 0.0) [(-1.0+sliceWidth/2),(-1.0+3*sliceWidth/2)..]
              splits = color (greyN 0.8) $ pictures [ line [(-1.0+(realToFrac n)*sliceWidth,-1),(-1.0+(realToFrac n)*sliceWidth,1)] | n <- [0..(nSlices-1)]]

verticalSplit :: [Plot] -> Plot
verticalSplit = Plot . VerticalSplit


data HorizontalSplit = HorizontalSplit [Plot]
-- TODO how should you cap from rendering values higher and lower?



--    return $ pictures $ renderVariable <$> readReadings
--        where renderVariable readReadingsVariable = graph readReadingsVariable <> start readReadingsVariable
--              graph readReadingsVariable = color white $ line $ zip [-400,-395..] (take 512 readReadingsVariable)
--              start [] = mempty
--              start xv@(x:_) = translate (-400) x $ color red $ circle lastVariance
--                where lastVariance = sqrt $ (sum $ map (\x->(x-lastMean)^2) lastReadings) / (max 1 lastLength)
--                      lastMean = (sum lastReadings) / lastLength
--                      lastLength = fromIntegral $ length lastReadings
--                      lastReadings = take 8 xv



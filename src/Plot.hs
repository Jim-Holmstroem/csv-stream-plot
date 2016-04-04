module Plot ( Plot
            , EmptyPlot(..)
            , SimplePlot(..)
            , simplePlot
            , render
            , append
) where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Expression

-- TODO use those derivative of a list things that is used in xmonad for window switching
-- TogglePlot Key [Plot], toggle between plots on keys
-- KeyPlot (Map Key Plot), switch between plots on keys

-- TODO how to get window dimenions in gloss?

class Plot p where
    render :: p -> Picture
    append :: [Double] -> p -> p
    -- TODO should the arguments be the other way around?


data EmptyPlot = EmptyPlot
    deriving (Show, Eq)
instance Plot EmptyPlot where
    render _ = mempty
    append _ p = p


data SimplePlot = SimplePlot [Expression] [[Double]]
instance Plot SimplePlot where
    render (SimplePlot _ evaluations) = pictures $ renderEvaluation <$> evaluations
        where renderEvaluation evaluation = graph evaluation
              graph evaluation = color white $ line $ zip [-800, -795..] (take 256 $ map realToFrac evaluation)
    append values (SimplePlot exprs evaluations) = SimplePlot exprs $ zipWith (:) evaluation $ enforceDimension evaluation evaluations
        where
            evaluation = map (flip eval values) exprs
            enforceDimension left [] = map (const []) left
            enforceDimension _ right = right


simplePlot :: [Expression] -> SimplePlot
simplePlot exprs = SimplePlot exprs []

module Plot ( Plot
            , EmptyPlot(..)
) where

import Graphics.Gloss.Data.Picture

-- TODO use those derivative of a list things that is used in xmonad for window switching
-- TogglePlot Key [Plot], toggle between plots on keys
-- KeyPlot (Map Key Plot), switch between plots on keys

class Plot p where
    render :: p -> Picture
    append :: [Double] -> p -> p


data EmptyPlot = EmptyPlot
    deriving (Show, Eq)


instance Plot EmptyPlot where
    render _ = mempty
    append _ p = p

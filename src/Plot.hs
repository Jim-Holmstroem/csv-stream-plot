module Plot ( Plot
) where

import Graphics.Gloss.Data.Picture

-- TODO use those derivative of a list things that is used in xmonad for window switching
-- TogglePlot Key [Plot], toggle between plots on keys
-- KeyPlot (Map Key Plot), switch between plots on keys

class Plot p where
    render :: p -> Picture
    append :: [Double] -> p -> p

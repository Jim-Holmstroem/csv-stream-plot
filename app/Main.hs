import Core

main = runPlot "StreamPlotter" $ verticalSplit [ordinaryPlot [x0, x1, x2], ordinaryPlot [x6, x7, x8]]

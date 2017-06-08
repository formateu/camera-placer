library(profr)
library(ggplot2)
source("metaalgo.R")
samplePointsX <- c(1, 1, 3, 3, 5, 5, 4, 4, 8, 8, 10, 10)
samplePointsY <- c(1, 7, 7, 10, 10, 6, 6, 3, 3, 9, 9, 1)


Rprof(tmp <- tempfile())
main(samplePointsX, samplePointsY, 5, 3)
Rprof()
summaryRprof(tmp)

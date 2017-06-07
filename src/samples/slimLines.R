source("utils.R")

#slimlines
pointsX <- c(1,1, 2, 2,3,3, 4, 4,5,5, 6, 6,7,7, 8, 8,9,9, 10,10)
pointsY <- c(1,10,10,2,2,10,10,2,2,10,10,2,2,10,10,2,2,10,10, 1)

runTests("slimMap", pointsX, pointsY)

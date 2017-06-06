#Project : Camera Placer
#Authors : Przemyslaw Kopanski
#          Mateusz Forc
source("metaalgo.R")

#mediumMap
samplePointsX <- c(1, 1, 3, 3, 5, 5, 4, 4, 8, 8, 10, 10)
samplePointsY <- c(1, 7, 7, 10,10,6, 6, 3, 3, 9, 9,  1)
main(samplePointsX, samplePointsY, 5, 2,"mediumMap")


#slimlines
samplePointsX <- c(1,1, 2, 2,3,3, 4, 4,5,5, 6, 6,7,7, 8, 8,9,9, 10,10)
samplePointsY <- c(1,10,10,2,2,10,10,2,2,10,10,2,2,10,10,2,2,10,10, 1)
main(samplePointsX, samplePointsY, 5, 2,"slimLines")

#openSpace
samplePointsX <- c(1,1, 10,10)
samplePointsY <- c(1,10,10,1)
main(samplePointsX, samplePointsY, 10, 2,"openSpace")

#mediumMap2
samplePointsX <- c(1,1,3,3,8,8,10,10,13,13)
samplePointsY <- c(4,9,9,7,7,3,3, 7, 7, 1)
main(samplePointsX, samplePointsY, 5, 2,"mediumMap2")

#Project : Camera Placer
#Authors : Przemyslaw Kopanski
#          Mateusz Forc
source("metaalgo.R")

#mediumMap
samplePointsXmediumMap <- c(1, 1, 3, 3, 5, 5, 4, 4, 8, 8, 10, 10)
samplePointsYmediumMap <- c(1, 7, 7, 10,10,6, 6, 3, 3, 9, 9,  1)

#slimlines
samplePointsXslimLines <- c(1,1, 2, 2,3,3, 4, 4,5,5, 6, 6,7,7, 8, 8,9,9, 10,10)
samplePointsYslimLines <- c(1,10,10,2,2,10,10,2,2,10,10,2,2,10,10,2,2,10,10, 1)

#openSpace
samplePointsXopenSpace <- c(1,1, 10,10)
samplePointsYopenSpace <- c(1,10,10,1)

#mediumMap2
samplePointsXmediumMap2 <- c(1,1,3,3,8,8,10,10,13,13)
samplePointsYmediumMap2 <- c(4,9,9,7,7,3,3, 7, 7, 1)


for (dp in seq(0, 1, 0.1)) {
    for (dk in seq(0, 1, 0.1)) {
        main(samplePointsXmediumMap,
             samplePointsYmediumMap,
             5, 2, dp, dk, paste("mediumMap", dp, dk, sep = "_"))
        main(samplePointsXslimLines,
             samplePointsYslimLines,
             5, 2, dp, dk, paste("slimLines", dp, dk, sep = "_"))
        main(samplePointsXopenSpace,
             samplePointsYopenSpace,
             10, 2, dp, dk, paste("openSpace", dp, dk, sep = "_"))
        main(samplePointsXmediumMap2,
             samplePointsYmediumMap2,
             5, 2, dp, dk, paste("mediumMap2", dp, dk, sep = "_"))
    }
}

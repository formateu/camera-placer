#Project : Camera Placer
#Authors : Przemyslaw Kopanski
#          Mateusz Forc
source("metaalgo.R")

#mediumMap2
pointsX <- c(1,1,1,3,3,8,8,10,10,13,13)
pointsY <- c(1,4,9,9,7,7,3,3, 7, 7, 1)

ani.options(interval = 0.4)

#parameters:
#vector of X coords of points (elements must be integer >=1)
#vector of Y coords of points (elements must be integer >=1)
#scale of accuracy - bigger means (integer, >=1)
# dp parameter, described in docs - 0 < dp <= 1
# dk parameter, described in docs - 0 < dk <= 1
# prefix of plot file (string)
main(pointsX, pointsY,
     5, 1, 1, 0.7, "main")

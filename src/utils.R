source("metaalgo.R")

# runs tests with multiple different parameters
runTests <- function(name, pointsX, pointsY) {
  for (dp in seq(0, 1, 0.1)) {
    for (dk in seq(0, 1, 0.1)) {
      print(paste("dp:", dp, "dk:", dk))
      main(pointsX, pointsY,
           5, 2, dp, dk,
           paste(name, dp, dk, sep = "_"))
      }
  }
}

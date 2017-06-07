source("metaalgo.R")

# runs tests with multiple different parameters
runTests <- function(name, pointsX, pointsY) {
  for (dp in seq(0.1, 1, 0.1)) {
    for (dk in seq(0.1, 1, 0.1)) {
      for (scale in c(2,5)) {
        print(paste("dp:", dp, "dk:", dk, "scale:", scale))
        main(pointsX, pointsY,
             scale, 2, dp, dk,
             paste(name, dp, dk, scale, sep = "_"))
      }
    }
  }
}

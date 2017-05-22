# q - funkcja celu
anealling <- function(initialPointGenerator, calculateTemperature
                      , selectRandomNeighbour, runningFunc, q, consumerFunc) {
  k <- 0
  x <- initialPointGenerator()
  while (runningFunc(k)) {
    t <- calculateTemperature(k)
    y <- selectRandomNeighbour(x)
    if (q(y) > q(x))
      x <- y
    else if (runif(1, 0, 1) < exp(-abs(q(y)-q(x)/t)))
      x <- y

    # draws the point on the graph and constructs graph
    consumerFunc(x, q(x))

    k <- k + 1
  }
}


keep_running <- function (k) {
  k < 10000
}


# map - matrix representing a map
# mapfield - number representing empty field in building plan
# solution - list of cameras places (vector of 2element vectors)
# radius - camera sight radius
# note: all of these parameters must be in identical scale
calculateCovering <- function(map, mapfield, radius, solution) {
  # value 1 represents plan's border (walls)
  # value 2 in map represents field outside building plan
  # value 3 respresents empty field inside the building plan

  cameraFieldCount <- 0L

  for (camera in solution) {
    x <- camera[1]
    y <- camera[2]

    # brute force filled circle drawing
    for (y1 in -radius:radius) {
      for (x1 in -radius:radius) {
        if (!is.na(map[x+x1,y+y1]) # check if location in scope
            && map[x+x1,y+y1] == 3 # check if field can be colored
            && x1*x1+y1*y1 <= radius*radius) { # check field is in camera's range
          map[x+x1,y+y1] <- 4
          cameraFieldCount <- cameraFieldCount + 1
        }
      }
    }
  }

  return(cameraFieldCount/mapfield)

}


# simple test

sampleMap <- matrix( c(2,2,2,2,2,
                       1,1,1,1,1,
                       1,3,3,3,1,
                       1,3,3,3,1,
                       1,3,3,3,1,
                       1,3,3,3,1,
                       1,1,1,1,1), nrow = 7, ncol = 5, byrow=TRUE)

print(calculateCovering(sampleMap, 12L, 1L, list(c(4L,3L))))
print(sampleMap)
print(dim(sampleMap))


goalFunction <- function(kmin, dp, dk, coveringFunc) {
  function(x) {
    k <- length(x)
    p <- coveringFunc(x)

    return(dp*p - 100*max(k, kmin)/kmin)
  }
}



generateRandomNeighbour <- function(map) {
  function(solution) {
# move one camera, add one camera or remove one camera randomly
# make the probability of moving one camera significally higher than any other option
# p_move > p_add > p_remove
# map is needed to check if new position is inside the building
    los <- runif(1, 0, 1)
    if (los < 0.8) {
      # move camera
      cameraIter <- sample(1:lengths(solution), 1)
      repeat {
        Xpos <- solution[cameraIter][1]
        Xpos <- Xpos + sample(-1:1, 1)
        Ypos <- solution[cameraIter][2]
        Ypos <- Ypos + sample(-1:1, 1)
        if (!is.na(map[Xpos, Ypos])
            && map[Xpos, Ypos] == 3
            && !is.element(c(Xpos, Ypos), solution))
          break;
      }
      solution[cameraIter][1] <- tmpXpos
      solution[cameraIter][2] <- tmpYpos

    } else if (los < 0.95) {
      # add camera
      Xdim <- dim(map)[1]
      Ydim <- dim(map)[2]
      repeat {
        Xpos <- sample(1:Xdim, 1)
        Ypos <- sample(1:Ydim, 1)
        if (map[Xpos, Ypos] == 3
            && !is.element(c(Xpos, Ypos), solution))
          break;
      }

    } else {
      # remove camera
      cameraIter <- sample(1:lengths(solution), 1)
      solution[cameraIter] <- NULL
    }
    return(solution)
  }
}


# calculates temperature based on current iteration number
# 300 is T0
# 10000 is expected number of iterations
# look around for other functions than exp
temperatureFunction <- function(param) {
  function(currentIteration) {
    300 * exp(-currentIteration/10000)
  }
}

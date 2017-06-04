PriorityQueue <- function() {
  keys <- values <- NULL
  curBest <- 0

  top <- function() { values[[curBest]] }

  insertUnique <- function(key, val) {
    if (length(values) == 0) {
      keys <<- list(key)
      values <<- list(val)
      curBest <<- 1
    } else {
      flag <- TRUE
      for (elem in values) {
        if (compareLists(elem, val)) {
          flag <- FALSE
          break
        }
      }
      if (flag) {
        values[[length(values)+1]] <<- val
        keys[[length(keys)+1]] <<- key
        if (key > keys[[curBest]]) {
          curBest <<- length(keys)
        }
      }
    }
  }
  environment()
}

compareLists <- function(l1, l2) {
  lengthsComparison <- length(l1) == length(l2)
  intersectNum <- length(intersect(l1, l2))
  if (length(l1) == length(l2)) {
    if (intersectNum == length(l1)) {
      TRUE
    }
  }
  FALSE
}

# q - funkcja celu
anealling <- function(initialPointGenerator, calculateTemperature,
                      selectRandomNeighbour, runningFunc, q, consumerFunc) {
  k <- 0
  x <- initialPointGenerator()
  log <- PriorityQueue()
  #log$insertUnique(q(x), x)
  while (runningFunc(k)) {
    print(k)

    t <- calculateTemperature(k)
    y <- selectRandomNeighbour(x)

    if (q(y) > q(x))
      x <- y
    else if (runif(1, 0, 1) < exp(-abs((q(y)-q(x))/t)))
      x <- y

    consumerFunc(k, x, q(x))
    #log$insertUnique(q(x), x)
    # draws the point on the graph and constructs graph

    k <- k + 1
  }
}


keep_running <- function(k) {
  k < 2000
}

isInScope <- function(xmax, ymax) {
  function(x, y) {
    x > 0 && x <= xmax && y > 0 && y <= ymax
  }
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

  inScope <- isInScope(dim(map)[1], dim(map)[2])

  for (camera in solution) {
    x <- camera[1]
    y <- camera[2]

    # brute force filled circle drawing
    for (y1 in -radius:radius) {
      for (x1 in -radius:radius) {
        if (inScope(x+x1, y+y1) # check if location in scope
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



goalFunction <- function(kmin, dp, dk, coveringFunc) {
  function(x) {
    k <- length(x)
    p <- coveringFunc(x)

    return(dp*p - dk*100*max(k, kmin)/kmin)
  }
}

generateRandomNeighbour <- function(map) {
  function(solution) {
    # move one camera, add one camera or remove one camera randomly
    # make the probability of moving one camera significally higher than any other option
    # p_move > p_add > p_remove
    # map is needed to check if new position is inside the building
    los <- runif(1, 0, 1)
    inScope <- isInScope(dim(map)[1], dim(map)[2])
    if (length(solution) != 0 && los < 0.95) {
      # move camera
      cameraIter <- sample(1:length(solution), 1)
      repeat {
        Xpos <- solution[[cameraIter]][1]
        Xpos <- Xpos + sample(-1:1, 1)
        Ypos <- solution[[cameraIter]][2]
        Ypos <- Ypos + sample(-1:1, 1)
        if (inScope(Xpos, Ypos)
            && map[Xpos, Ypos] == 3
            && !is.element(c(Xpos, Ypos), solution)) {
            solution[[cameraIter]][1] <- Xpos
            solution[[cameraIter]][2] <- Ypos
            break
        }
      }

    } else if (length(solution) == 0 || los < 0.99) {
      # add camera
      Xdim <- dim(map)[1]
      Ydim <- dim(map)[2]
      repeat {
        Xpos <- sample(1:Xdim, 1)
        Ypos <- sample(1:Ydim, 1)
        if (map[Xpos, Ypos] == 3
            && !is.element(c(Xpos, Ypos), solution)) {
            solution[[length(solution)+1]] <- c(Xpos, Ypos)
          break
        }
      }

    } else {
      # remove camera
      cameraIter <- sample(1:length(solution), 1)
      solution[[cameraIter]] <- NULL
    }
    return(solution)
  }
}

# calculates temperature based on current iteration number
# 300 is T0
# 2000 is expected number of iterations
# look around for other functions than exp
temperatureFunction <- function(param) {
  function(currentIteration) {
    300 * exp(-currentIteration/2000)
  }
}

generateMap <- function(pointsX, pointsY, scale) {
  xPositions <- sapply(pointsX, function(point) {
                         point*scale
            })
  yPositions <- sapply(pointsY, function(point) {
                         point*scale
            })

  #for matrix creation and optimization of map size
  minX <- min(xPositions)
  minY <- min(yPositions)
  maxX <- max(xPositions)
  maxY <- max(yPositions)

  map <- matrix(2L, nrow=maxX, ncol=maxY)

  for (pos in 1:length(xPositions)) {
    map[xPositions[pos], yPositions[pos]] <- 1
  }

  xPositions[[length(xPositions) + 1]] <- xPositions[1]
  yPositions[[length(yPositions) + 1]] <- yPositions[1]

  for (index in 2:length(xPositions)) {
    point1 <- c(xPositions[index-1], yPositions[index-1])
    point2 <- c(xPositions[index], yPositions[index])

    diffX <- point1[1] - point2[1]
    diffY <- point1[2] - point2[2]

    if (diffX != 0) {
      for (xPos in point1[1]:point2[1]) {
        map[xPos, point2[2]] <- 1
      }
    } else {
      for (yPos in point1[2]:point2[2]) {
        map[point2[1], yPos] <- 1
      }
    }
  }

  xPositions <- head(xPositions, -1)
  yPositions <- head(yPositions, -1)

  flag <- 0
  lastVal <- 4

  for (y in 1:dim(map)[2]) {
    for (x in 1:dim(map)[1]) {
      if (flag == 1 && map[x,y] == 1) {
        if (lastVal == 1) {}
        else if (lastVal != 1) { flag <- 0 }
      } else if (flag == 1 && map[x,y] != 1) {
        map[x,y] <- 3
      } else if (map[x,y] == 1){ #flag == 0
        flag <- 1
      }
      lastVal <- map[x,y]
    }
    flag <- 0
    lastVal <- 4
  }

  return(map)
}

countMapField <- function(map) {
  mapField <- 0
  for (y in 1:dim(map)[2]) {
    for (x in 1:dim(map)[1]) {
      if (map[x,y] == 3) { mapField <- mapField +1 }
    }
  }
  mapField
}

# simple test

if (FALSE) {
"
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
"
}

generateInitState <- function (map, camNum) {
  if (FALSE) {
    "
    generates vector of initial camera positions
    for given map and camera number
    in allowed space (inside stage)
    "
  }
  result <- list()
  dimX <- dim(map)[1]
  dimY <- dim(map)[2]

  actCamNum <- 0
  i <- 1

  repeat {
    repeat {
      Xpos <- sample(1:dimX, 1)
      Ypos <- sample(1:dimY, 1)
      point <- c(Xpos, Ypos)

      if (map[Xpos, Ypos] == 3 && !(point %in% result)) {
        result[[i]] <- point
        i <- i + 1
        break
      }
    }

    actCamNum <- actCamNum + 1

    if (actCamNum == camNum) { break }
  }

  return(result)
}

initGlobals <- function() {
  plotDataIter <<- c()
  plotDataGoal <<- c()
  #plotDataSolution <<- c()
}

# k - nr iteracji
# x - rozwiazanie
# q - wartosc f celu dla rozwiazania
consumeNewData <- function(k, x, q) {
  plotDataIter <<- c(plotDataIter, k)
  plotDataGoal <<- c(plotDataGoal, q)
  #plotDataSolution <<- c(plotDataSolution, x)
}

drawGraphs <- function() {
  plot(plotDataIter, plotDataGoal, main="Wartosc f celu", type="l",
       xlab="Nr iteracji", ylab="Funkcja celu")
}

main <- function(xPoints, yPoints, scale, cameraRadius) {
  initGlobals()
  map <- generateMap(xPoints, yPoints, scale)
  mapField <- countMapField(map)
  cameraRadius <- scale * cameraRadius
  cameraField <- pi * cameraRadius * cameraRadius
  cameraNumber <- ceiling(mapField / cameraField)
  print(cameraNumber)
  anealling(function() {generateInitState(map, cameraNumber)},
            temperatureFunction(0),
            generateRandomNeighbour(map),
            keep_running,
            goalFunction(cameraNumber, 1, 1, function(x) {
                           calculateCovering(map,mapField, cameraRadius, x)
                       }),
            consumeNewData)
  drawGraphs()
}



samplePointsX <- c(1, 1, 3, 3, 5, 5, 4, 4, 8, 8, 10, 10)
samplePointsY <- c(1, 7, 7, 10, 10, 6, 6, 3, 3, 9, 9, 1)

main(samplePointsX, samplePointsY, 5, 3)

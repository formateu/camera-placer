require(compiler)
enableJIT(3)

Buffer <- function(inSize) {
    queue <- list()
    maxSize <- inSize
    size <- 0

    push <- function(elem) {
        if (size == maxSize) {
            queue <<- tail(queue, -1)
            queue <<- c(queue, list(elem))
        } else if (size < maxSize) {
            queue[[size + 1]] <<- elem
            size <<- size + 1
        }
    }

    pop <- function() {
        if (size >= 0) {
            elem <- head(queue)
            queue <<- tail(queue, -1)
            size <<- size - 1
            elem
        }
    }

    find <- function(elem) {
        for (solution in queue) {
            if (compareLists(elem, solution) == TRUE) {
                TRUE
            }
        }
        FALSE
    }

    #for test purposes
    printBuf <- function() { print(queue) }

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
                      selectRandomNeighbour, runningFunc, q, consumerFunc, tabooSize) {
  k <- 0
  x <- initialPointGenerator()
  taboo <- Buffer(tabooSize)
  while (runningFunc(k)) {
    #print(k)

    t <- calculateTemperature(k)
    repeat {
        y <- selectRandomNeighbour(x)

        if (!taboo$find(y)) { break }
    }


    # for optimization
    goalX <- q(x)
    goalY <- q(y)

    # draws the point on the graph and constructs graph
    consumerFunc(k, x, goalX, goalY)

    if (goalY > goalX)
      x <- y
    else if (runif(1, 0, 1) < exp(-abs((goalY-goalX)/t)))
      x <- y

    taboo$push(x)

    k <- k + 1
  }
}


keep_running <- function(numberofiters) {
  function(k) {
    k < numberofiters
  }
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
        if (x1*x1+y1*y1 <= radius*radius # check field is in camera's range
            && inScope(x+x1, y+y1) # check if location in scope
            && map[x+x1,y+y1] == 3) { # check if field can be colored
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

    return(dp*100*p - (dk*100*max(k-kmin, 0)/kmin))
  }
}

generateRandomNeighbour <- function(map) {
  inScope <- isInScope(dim(map)[1], dim(map)[2])
  function(solution) {
    # move one camera, add one camera or remove one camera randomly
    # make the probability of moving one camera significally higher than any other option
    # p_move > p_add > p_remove
    # map is needed to check if new position is inside the building
    los <- runif(1, 0, 1)
    if (length(solution) != 0 && los < 0.998) {
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

    } else if (length(solution) == 0 || los < 0.9995) {
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
# look around for other functions than exp
temperatureFunction <- function(param, expectedIters) {
  function(currentIteration) {
    param * exp(-currentIteration/expectedIters)
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
  plotDataGoalGenerated <<- c()
  plotDataSolution <<- c()
}

# k - nr iteracji
# x - rozwiazanie
# q - wartosc f celu dla rozwiazania
consumeNewData <- function(k, x, qx, qy) {
  plotDataIter <<- c(plotDataIter, k)
  #plotDataSolution <<- c(plotDataSolution, x)
  plotDataGoal <<- c(plotDataGoal, qx)
  plotDataGoalGenerated <<- c(plotDataGoalGenerated, qy)
}

drawGraphs <- function() {
  jpeg('plot.jpg')
  par(mfrow=c(2,1))
  plot(plotDataIter, plotDataGoal, main="Wartość funkcji celu aktualnie wybranego punktu", type="l",
       xlab="Nr iteracji", ylab="Funkcja celu")
  plot(plotDataIter, plotDataGoalGenerated, main="Wartość funkcji celu wygenerowanego punktu", type="l",
       xlab="Nr iteracji", ylab="Funkcja celu")
}

main <- function(xPoints, yPoints, scale, cameraRadius) {
  iternums <- 5000
  temperatureFuncFactor <- 0.5
  dp <- 1
  dk <- 1
  tabooSize <- 5
  initGlobals()
  map <- generateMap(xPoints, yPoints, scale)
  mapField <- countMapField(map)
  cameraRadius <- scale * cameraRadius
  cameraField <- pi * cameraRadius * cameraRadius
  cameraNumber <- ceiling(mapField / cameraField)
  anealling(function() { generateInitState(map, cameraNumber) },
            temperatureFunction(temperatureFuncFactor, iternums),
            generateRandomNeighbour(map),
            keep_running(iternums),
            goalFunction(cameraNumber, dp, dk, function(x) {
                           calculateCovering(map, mapField, cameraRadius, x)
                       }),
            consumeNewData,
            tabooSize)
  drawGraphs()
}

source("metaalgo.R")

solution <- list()
solution[[1]] <- c(1,2)
solution[[2]] <- c(3,2)
solution[[3]] <- c(4,7)
solution[[4]] <- c(0,9)
solution[[5]] <- c(3,4)

solution2 <- list()
solution2[[1]] <- c(2,2)
solution2[[2]] <- c(3,2)
solution2[[3]] <- c(4,7)
solution2[[4]] <- c(0,9)
solution2[[5]] <- c(3,4)


solution3 <- list()
solution3[[1]] <- c(3,3)
solution3[[2]] <- c(3,3)
solution3[[3]] <- c(4,7)
solution3[[4]] <- c(0,9)
solution3[[5]] <- c(3,4)

buf <- Buffer(2)
print("after first push")
buf$push(solution)
buf$printBuf()

print("after second push")
buf$push(solution2)
buf$printBuf()

print("after third push")
buf$push(solution3)
buf$printBuf()

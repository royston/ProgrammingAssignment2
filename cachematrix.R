## the two functions here help you 1. create a special matrix object that can cache its inverse and 2. computes the inverse of a matrix and returns a cached version of the inverse, if available. 
## This function encapsulates a matrix and its inverse. it provides functions to get the matrix or its inverse and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  getMatrix <- function() x
  getInverse <- function() i
  setInverse <- function(inverse) i <<- inverse
  
  list(setInverse = setInverse, getInverse = getInverse, getMatrix = getMatrix)
}


## This function finds the inverse of a matrix, if it hasn't already been solved. If it has already been solved, it returns the solved version of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data)
  x$setInverse(i)
  i
}

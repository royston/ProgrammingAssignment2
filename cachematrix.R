## the two functions here help you 1. create a special matrix object that can cache its inverse and 2. computes the inverse of a matrix and returns a cached version of the inverse, if available. 
## This function encapsulates a matrix and its inverse. it provides functions to get the matrix or its inverse and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  getMatrix <- function() x
  getInverse <- function() inverse
  setInverse <- function(i) inverse <<- i
  
  list(setInverse = setInverse, getInverse = getInverse, getMatrix = getMatrix)
}


## This function finds the inverse of a matrix, if it hasn't already been solved. If it has already been solved, it returns the solved version of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}

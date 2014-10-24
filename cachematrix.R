## This script was created in order to satisfy the requirements of week 3's programming assignment.
## The assignment is to write a pair of functions that cache the inversion of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## The matrix supplied is assumed always to be invertible, as per the assignment.
makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL

  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }

  # get the value of the inversion
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  # returns the product of this function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()
  # get the inverse, if it exists
  if (!is.null(matrixinverse)) {
    message("returning cached data")
    return(matrixinverse)
  }
  # otherwise, the inverse will be made, stored and returned
  matrix <- x$get()
  matrixinverse <- solve(matrix, ...)
  x$setinverse(matrixinverse)
  message("caching returned data")
  matrixinverse
}

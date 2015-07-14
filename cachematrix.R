## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Following are a pair of functions that cache the inverse
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inversed <<- solve
  getInverse <- function() inversed
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversed <- x$getInverse()
  if (!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setInverse(inversed)
  inversed
}

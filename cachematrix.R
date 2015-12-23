## Functions for computing an inverse of a matrix
## with caching functionality.

## Creates and returns a vector of functions
## for getting and setting a matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  getInverse <- function() i
  setInverse <- function(inverse) {
    i <<- inverse
  }
  list(get=get, set=set, 
       getInverse = getInverse,
       setInverse = setInverse)
}


## Retrieves the inverse of the given matrix x.
## If the inverse was computed before it has been 
## cached and this function gets the cached inverse.
## This function assumes that the inverse of x exists.
## Note that x has to be of type 'cache matrix' as 
## constructed by makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix)
  x$setInverse(i)
  i
}

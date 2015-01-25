## The functions in this file are constructed to cache the inverse of a matrix.
## This may be useful in minimising time-consuming computations when the inverse of a matrix has to be computed repeatedly 
## (as in a loop) and the contents of the matrix do not change.In this case it may make sense to cache the value of the 
## inverse of the matrix so that when it is needed again it can be looked up in the cache.

## The function makeCacheMatrix creates a list of functions that can set and retrieve the values of a matrix
## and set and retrieve the values of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function will check to see if the inverse of the matrix it is given has already been calculated.
## If it has already been calculated it will retrieve the inverse from the cache. Otherwise it will calculate the inverse 
## and store it in the cache for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
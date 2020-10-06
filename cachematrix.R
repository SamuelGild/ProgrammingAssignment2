
## This pair of functions serve to cache the inverse of a matrix


## makeCacheMatrix() serves to create a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve() serves to compute the inverse of the "matrix" returned
## by makeCacheMatrix(), or to retrieve the inverse from the cache if 
## the inverse has already been calculated

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
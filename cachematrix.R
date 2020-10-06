
## This pair of functions serve to cache the inverse of a matrix


## makeCacheMatrix() serves to create a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## CacheSolve() serves to compute the inverse of the "matrix" returned
## by makeCacheMatrix(), or to retrieve the inverse from the cache if 
## the inverse has already been calculated

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
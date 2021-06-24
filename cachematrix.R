## This file has two functions:
##  makeCacheMatrix: 
##  This function creates a special "matrix" 
##  object that can cache its inverse.
##  cacheSolve: 
##  This function computes the inverse of the 
##  special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated 
##  (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.

## This function creates a special square matrix in list form.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) inverse <<- mean
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function checked to see if the inverse of the matrix has been cached or
##it calculates the inverse of a matrix if it has not been cached.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

## This file contains two functions: one which creates the matrix and
## the other one which solves the matrix. It makes uses of caches in
## order to save computing time, so if a matrix has already been solved before,
## it does not re-compute the results again, but simply pulls them from the cache.
## (Written by Punit Dharmadhikari http://github.com/punitdharmadhikari/)

## The following function creates a cache matrix and creates the set and get values for the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <- function() m
  
  z <- list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function searches the environment to see if the matrix has already been solved.
## If so, it returns the solution.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInverse(data)
  x
}
## Assignment description from Coursera
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## For this assignment, assume that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse
## based heavily on the makeVector function specified as example

## has 4 internal functions: 
## set        -- initializing a matrix
## get        -- reading matrix 
## setinverse -- setting a cached inverse
## setinverse -- reading a cached inverse
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x

  ## setting the inverse (used by cacheSolve)
  setinverse <- function(inverse) m <<- inverse

  ## getting the inverse (used by cacheSolve)
  getinverse <- function() m
  
  ## listing up the functions of the object, so they're available to call externally
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## based heavily on the cachemean function specified as example

cacheSolve <- function(x, ...) {
  ## returns a matrix that is the inverse of 'x'

  ## return data if already cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## calculate inverse if not cached
  data <- x$get()
  m <- solve(data, ...)

  ## save calculated inverse to cache
  x$setinverse(m)
  m
  
}

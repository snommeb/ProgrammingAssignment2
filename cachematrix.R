## The "makeCacheMatrix" creates a special matrix object
## that can cache its inverse. It returns a list object
## containing several functions that can be used to 
## manipulate and retrieve the matrix and its inverse.
## The "cacheSolve" function computes the inverse of the
## matrix created by the "makeCacheMatrix". It uses
## caching to retrieve the inverse from the cache if it has
## already been calculated but if the matrix has changed 
## since, "cacheSolve" will recalculate the inverse and
## update cache.
##
##
## This function creates a special matrix object that can
## cache its inverse and provides methods for setting and
## getting the matrix, setting and gettings its inverse
## and caching the inverse for future use.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  get <- function() x
  
  cache_inverse <- function() {
    if (!is.null(inv)) {
      
      inv
    } else {
      
      inv <<- solve(x)
      inv
    }
  }
  
  
  get_inverse <- function() inv
  
  
  list(set = set, get = get, cache_inverse = cache_inverse, get_inverse = get_inverse)
  
}


## This function takes matrix 'x' and checks if its inverse
## is already cached. If it is then the function will 
## retrieve cached value. If the inverse is not cached it
## computes using the 'solve' function, caches the result
## using the 'setinverse' function from 'makeCacheMatrix' 
## and returns the inverse.

cacheSolve <- function(x, ...) {
  
  n1 <- x$getinverse()
  if(!is.null(n1)) {
    message("getting cached data")
    return(n1)
  }
  
  m1 <- x$get()
  n1 <- solve(m1, ...)
  
  x$setinverse(n1)
  
  n1
}

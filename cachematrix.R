## These two functions create a special matrix that allows the inverse to be cached
## and returned without requiring recalculation.

## This function creates a special matrix that can be used to calculate 
## the inverse and cache the inverse incase the inverse is needed again

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #create cache variable
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## Looks for a cached inverse of matrix and if it doesn't exist, calculates the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) ##if the inverse is cached
  {
    message("getting cached data")
    return (i)
  }
  data <- x$get()  ## if the inverse is not cached
  i <- solve(data, ...)  ## Calculate the inverse
  x$setInverse(i)       ## Cache the inverse
  i
}

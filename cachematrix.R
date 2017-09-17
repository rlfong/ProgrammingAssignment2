## Put comments here that give an overall description of what your
## functions do

## My attempt at a function that creates a super special matrix that can cache its 
## equally awesome inverse.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Assuming the previous function worked as planned, I will now attempt to compute 
## the inverse of the special matrix.  If already calculated, the inverse will be retrieved
## from the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

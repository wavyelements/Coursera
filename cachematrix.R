## Put comments here that give an overall description of what your
## functions do:

# The first function creates an object used to cache results of matrix
# operations, in this case the inverse. The second function performs
# the calculation if it has not already been stored in the cache. If 
# it has already been performed, the output is from what is stored in
# the cache.

## This creates a special matrix for the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes inverse of original matrix input using values already 
## computed if available

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

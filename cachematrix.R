#Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of 
# a matrix rather than compute it repeatedly 
# First function creates list object creating matrix
# Second function get inverse if it was calculated before
# or calculate it if it was not calculated before

## This function creates a special "matrix"
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## get inverse if it exists or calculate if it was not 
# calculated previously

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



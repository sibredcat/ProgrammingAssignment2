## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinv <- function(inv) r <<- inv
  getinv <- function() r
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then cacheSolve retrievs the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  r <- x$getinv()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }
  data <- x$get()
  r <- solve(data, ...)
  x$setinv(r)
  r
}

## Functions below are written to cache the inverse of a matrix,
## assuming that the matrix supplied is always invertible.


##  This function creates a special "matrix" object that can cache
##  its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixsolve <- function(matrixsolve) m <<- matrixsolve
  getmatrixsolve <- function() m
  list(set = set, get = get,
       setmatrixsolve = setmatrixsolve,
       getmatrixsolve = getmatrixsolve)
}

##  This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already
##  been calculated (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixsolve(m)
  m
}


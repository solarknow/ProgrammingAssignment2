## This is a set of functions that calculate the inverse of matrix and caches it for later use. 

## creates a "Matrix" whose inverse will be cached. 
## essentially returns a list of (get,set) functions for the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  ##Setting up get/set fuctions for the matrix
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  ##Setting up get/set functions for inverse of matrix
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  ##Returning list of get/set functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## returns the inverse of x. first checks if the inverse is cached. if so, returns it from cache, if not, it calculates the inverse,
## caches it and returns it. 

cacheSolve <- function(x, ...) {
  ## Checking if inverse of 'x' is cached; if so, display a message and return it
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## If not, retrieve the matrix, calculate the inverse, set it and return it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

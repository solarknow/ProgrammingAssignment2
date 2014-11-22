## This is a set of functions that calculate the inverse of matrix and caches it for later use. 

## creates a "Matrix" whose inverse will be cached. 
## essentially returns a list of (get,set) functions for the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## returns the inverse of x. first checks if the inverse is cached. if so, returns it from cache, if not, it calculates the inverse,
## caches it and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

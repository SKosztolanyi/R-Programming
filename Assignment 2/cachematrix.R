## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
## This function uses operator <<- which can assign a value to an operator from different environment.
## Fhis function creates special object that stores a matrix and cache's its inverse.
## Given the matrix doesn't change, it is better to store it's inversion in cache.

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


## Write a short comment describing this function

## The second function checks if the inversion has already been calculated.
## If the inversion wasn't calculated, it calculates the value.
## If the inversion was calculated, it is stored in a cache and is not calculated anew.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s      ## Return a matrix that is the inverse of 'x'
}

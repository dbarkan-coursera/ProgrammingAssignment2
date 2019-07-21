## This R file provides functions for reading a matrix and
## computing the inverse of the matrix. To speed up this computation,
## the ability to cache the result is provided, and a second call to
## the inverse computation utility will return the cached result instead
## of rerunning the full inverse computation.

## Example of running:
## > source('matrixCache.R')

## Create the matrix and adjust it to avoid inverse function errors
## > m <- matrix(1:16, 4, 4)
## > m[3, 4] <- 8
## > m[2, 3] <- 7

## Create the caching utility and set the matrix
## > newM <- makeCacheMatrix()
## > newM$set(m)

## This call will calculate the inverse and cache it
## > cacheSolve(newM)

## Calling this function again returns the cached value and prints a message indicating such
## > cacheSolve(newM)

## If newM$set() is called again with an update matrix, the inverse calculation will be performed again.




## This function creates a special list that serves as an inverse calculator
## The elements are the following functions, in order:
## set -- sets a matrix
## get -- returns the matrix that is set
## setsolve -- set the value of the inverse of m
## getsolve -- get the value of the inverse of m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function takes the list returned by makeCacheMatrix as input
## It returns the inverse of the matrix, either through caching or
## a new calculation as described above.
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}



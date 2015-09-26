## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that stores a list of functions: get, set,
## getinvmatrix, and setinvmatrix.
## When makeCacheMatrix is stored as an object, we can call each function
## as a subset of that object.

## We can *set* a matrix to be solved by cachesolve, we can *get* this
## matrix back.

## We can also save the solution directly using setinvmatrix. This
## returns a value of m so that cacheSolve will use this existing
## solution. Like set and get, we can pull the solution from this
## function using getinvmatrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get = function () x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list(set=set, getinvmatrix=getinvmatrix, setinvmatrix=setinvmatrix, get=get)  
}


## CacheSolve either 1) Returns the cached inverted/solved matrix or (else) 2) calculates
## the inverted matrix with a solve function and passes the argument to setinvmatrix.

## By calling the function setinvmatrix, the next time cacheSolve is ran it will return
## a cached matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix() ##is there a invmatrix already stored?
        
  if(!is.null(m)) {
    message("Getting Inverse Matrix. Warning: Inverse matrix may have been 
            calculated in cacheSolve, or it was created by the function
            setinvmatrix()")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}

## We are going to write two functions to calculate and cache
## the inverse of a given and invertible matrix.

## This function, makeCacheMatrix, creates a special "matrix" object, 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix's inverse
## get the value of the matrix's inverse
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve function first check if the inverse 
## of the matrix has already been calculated, in which case it gets it from the cache.
## Otherwise it calculates the inverse of the matrix and returns this value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
      }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
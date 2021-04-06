## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a list object with 4 functions set, get, setinverse, 
## getinverse. It includes the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function stores an in-memory inverse matrix in case it has not been computed.
## With a makeCacheMatrix as an argument. Additionally, it returns the inverse matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

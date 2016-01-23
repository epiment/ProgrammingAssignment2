# function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  x_1 <- NULL
  set <- function(y) {
    x <<- y
    x_1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_1 <<- inverse
  getinverse <- function() x_1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# function cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    x_1<- x$getinverse()
    if(!is.null(x_1)) {
      message("getting cached data.")
      return(x_1)
    }
    data <- x$get()
    x_1 <- solve(data)
    x$setinverse(x_1)
    x_1
}
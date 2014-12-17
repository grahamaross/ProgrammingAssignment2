## Programming assignment 2
## Caching the inverse of a matrix to avoid unnecessary recalculations.

## Create a list of functions for operating on the cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Tests:

## Create a local matrix "m2" consisting of (2, 0, 0, 2).
## m2 <- matrix(c(2,0,0,2),2,2)

## Use makeCacheMatrix to create a local variable "mcm" representing m2.
## mcm <- makeCacheMatrix(m2)

## The inverse of this matrix will be (0.5, 0, 0, 0.5), as shown thus:
## solve(m2)
## solve(m2) %*% m2 # yields the identity matrix

## Use cacheSolve to invert the matrix thrice. On the second and third times a message indicates that the cache was used.
## cacheSolve(mcm)
## cacheSolve(mcm)
## cacheSolve(mcm)

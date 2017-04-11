## The next pair of functions take a matrix and return 
## the inverse of this matrix. The inverse of the array is cached. 
## If the inverse of the matrix has already been calculated, 
## the cache value is taken to return the result.
## It is assumed that the matrix can be calculated the inverse.

## makeCacheMatrix function receives as a parameter an matrix and returns a 
## list of functions applied to the matrix:
## - set: set the value of the matrix
## - get: get the value of the matrix
## - setInverse: set the inverse of the matrix
## - getInverse: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) i <<- Inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function compute the inverse of the matrix. 
## If the inverse of the matrix has already been calculated, 
## the cache value is taken to return the result. 
## It is assumed that the matrix can be calculated the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

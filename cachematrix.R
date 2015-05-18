## Functions to solve the inverse of a matrix with cached results.
## The target is to avoid re-calculating an already solved matrix 
## inverse by storing the calculated inverses in a cache object

## makeCacheMatrix creates an object encapsulating a matrix and its 
## cached inverse. It initializes the inverse to NULL and keeps the 
## value assigned through the setinverse method. Call to getinverse 
## will therefore return NULL when the inverse has not yet been 
## assigned or the stored value otherwise
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <- NULL
  }
  get <- function() x
  setinverse <- function(theinverse) inverse <<- theinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x', where
## x is an object created passing a matrix to the function 
## makeCacheMatrix. This functions tries first to get the stored
## inverse of the matrix. If that returns a not null value it 
## returns that result. Otherwise it solves the inverse of the 
## matrix and store it in the cache for future use
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
## Caching the inverse of a matrix can provide computational benefits as it tends be costly operation
## The following functions will allow us to do that

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## Initialize values for "matrix object"
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Return value of matrix
  get <- function() x
  
  ##set and get cached inverse 
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  matrix(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## If inverse has been cached then return that value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise calculate inverse, cache it, and return that value
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

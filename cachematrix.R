## This R functions are able to cache the inverse of a matrix rather than compute it repeatedly. Below are two functions 
## that are used to create a special object that stores a numeric matrix and cache's its inverse.


## makeCacheMatrix creates a special "matrix", which is a list containing a function to set the value of the matrix, get 
## the value of the matrix, set the value of the inverse and get the value of the inverse

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


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
## been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

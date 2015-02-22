## Those functions enable to compute the inverse of a given
## matrix and keep the result cached for further use

## Create a special "matrix" object that can cache its inverse
## It is a list of functions designed to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    y <<- x
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of the matrix x. Gets into the cache to
## see if it has already been calculated, in which case the cache
## is the answer. Otherwise the inverse is calculated and stored into
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

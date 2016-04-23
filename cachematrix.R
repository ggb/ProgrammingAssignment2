## The following two functions are used to store a special kind of matrix
## and to calculate the inverse of the matrix in an efficent, cached way.

## Returns a list of functions to get and set the contained matrix and to get
## and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks is the given matrix x already contains a pre-calculated inverse.
## If so, it returns this inverse. If not, the inverse is calculated and
## stored in x for later use.

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

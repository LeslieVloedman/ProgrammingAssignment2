## These two functions create a special object that stores a matrix
## and caches its inverse.

## This first function creates a list containing a function to
## 1 set the value of the matrix.  2 get the value of the matrix.
## 3 set the value of the inverse. 4 get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function checks whether the inverse has already been
## calculated.  If so, it gets the inverse from the cache.  If not, it
## calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

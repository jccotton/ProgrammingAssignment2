## These functions create a type of matrix object as a list that is capable
## of caching it's inverse so that the calculation doesn't need to be redone

## This function creates a special "matrix" which is really just a list 
## containing functions to set the value of the matrix, get the value of the
## matrix, set the value of the inverse, and get the value of the inverse

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


## This function returns the inverse of 'x', however before performing the 
## calculation, it checks to see if the answer has already been cached, if
## it has, then it returns the cached value, if it hasn't then it calculates
## the answer

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

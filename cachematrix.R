## Author: Derek Goss
## These functions allow you to pass in a square invertible matrix
## and upon the first creation of this matrix, it will cache the inverse
## of the matrix using the solve function.  If the matrix hasn't changed
## and is recalled, it will find the inverse from cache as opposed to re-applying
## the solve function on it again.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
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


## Computers the inverse of the special "matrix" returned by makeCacheMatrix.  If the inverse has already been
## calculated and hasn't changed, then it will return the inverse from cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)

        ## Return a matrix that is the inverse of 'x'
      m


}

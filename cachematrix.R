## makeCacheMatrix function creates a special vector with 
## functions to set the value of the matrix, getting the value of the matrix,
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m<- NULL
      set <- function(y) {
          x<<- y
          m<<- NULL
      }
      get<- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)
}

## cacheSolve function determines inverse of the matrix.
## It first checks to see if the inverse has already been calculated.
## If it is already calculated, it gets the inverse from the cache and skips computation.
## Otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if (!is.null(m)) {
		message("getting cached data")
		return(m)
	  }
	  data <- x$get()
        m<-solve(data,...)
	  x$setinverse(m)
 	  m

}

## These functions provide a simple way to invert a matrix avoiding any repeat of the inversion program 
## if it has already been done once, by caching the data

## makeCacheMatrix returns a list of functions to set and get the original and inverted matrix data


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = setmatrix, get = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cachedSolve checks if the inverted data is already stored in the cache, 
## calculates and stores the inverted matrix if it hasn't been done and returns the inverted matrix either way

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


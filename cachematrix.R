## These functions calculate the inverse of a matrix but first it checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache.

## makeCacheMatrix create a spetial matrix which is really a list containing a function to 
## set/get the value of the matrix and set/get its inverse   

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculate the inverse of the special "matrix" created with makeCacheMatrix. 

cacheSove <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'	
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

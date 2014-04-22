## With the following two functions - 'makeCacheMatrix' and 
## 'cacheSolve' - first you can create a matrix object, then
## calculate its inverse. If the matrix's inverse was already
## calculated - and the matrix has not changed - the function
## (to save time) will retrive the inverse from the cache and  
## won't run the computation. 

## This function - makeCacheMatrix - creates a special "matrix"
## object that can cash its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function - casheSolve - computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## the cacheSolve function will retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

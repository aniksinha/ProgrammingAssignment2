## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix - Creates a special matrix which returns a list containing a functions to set/get the value of the matrix and set/get the inverse of the matrix
##cacheSolve - Uses the special matrix created by makeCacheMatrix and returns the inverse from the cache if already calculated

## Write a short comment describing this function
##Creates special matrix which stores the cache of inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function returns the inverse of the matrix from the cache if the inverse has already been calculated and the matrix has not changed.
##else calculates the inverse of the matrix and adds to the cache
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

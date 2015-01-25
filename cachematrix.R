## The following functions provide capabilities to create and cache the inverse
## of a matrix allowing retrieval of the matrix from a cache rather than 
## generating the inverse every time it is needed.  The intent is to save time
## and compute resources by retrieving the saved value.

## makeCacheMatrix accepts a matrix and stores it as well as the inverse of the
## matrix if it has been created.  it provides the set and get methods to set and
## retrive the matrix and the setinverse and getinverse to do the same for the 
## inverse of the matrix

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


## cacheSolve accepts a matrix as an argument and returns the inverse from
## cache if it has already been created.  If not it calculates the inverse
## and stores it as well as returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(m)
  m
}

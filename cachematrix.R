## Caching the Inverse of a Matrix - For RDPeng's Programming Assignment #2
## Coursera R Programming language course from Sep 1, 2014

## The function caches the value of the inverse of the matrix, a time consuming
## computation, so that it can be fetched again without re-computation.
## A very important assumption here is that the matrix is inversible.

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

## The function returns the inverse of any given matrix x, while checking to
## make sure that the matrix is a square matrix

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

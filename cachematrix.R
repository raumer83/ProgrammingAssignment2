## Put comments here that give an overall description of what your
## functions do
# makeCachematrix() and cacheSolve() make it possible to cache a matrix and
# it's inverse.
#
# Usage example:
#
#   CM <- makeCacheMatrix()
#   CM$set(matrix(1:4,2,2))   # cache a matrix.
#   CM$get()                  # return the cached matrix.
#   cacheSovle(CM)            # computes the inverse of the cached matrix.
#                               or returns the cached inverse is already
#                               computed.
#   CM$setInverse(matInv)     # manually cache matInv as the inverse


## Write a short comment describing this function
# Creates a list of functions that can be used to:
#   (1) set the value of a chachable matrix
#   (2) get the value cached matrix
#   (3) set the inverse of a cached matrix
#   (4) get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
# cacheSolve() returns the inverse of a matrix created by makeCacheMatrix().
# If the matrix inverse is already cached, the cached matrix is returned. Else
# the inverse is calculated and then cached for future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}

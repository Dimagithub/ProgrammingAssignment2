## Put comments here that give an overall description of what your
## functions do

## create "object" of a type MATRIX
# add four "methods" - get, set, getinv, setinv
# parameter - may be skipped or existing matrix

makeCacheMatrix <- function(mt = matrix()) {
  mt.inverse <- NULL
  set <- function(y) {
    mt <<- y
    mt.inverse <<- NULL
  }
  get <- function() mt
  setinv <- function(inv) mt.inverse <<- inv
  getinv <- function() mt.inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns inverse of the parameter matrix
# assumption - the given matrix is invertible
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #check if cache does not exist - then create it
  if (is.null(inv)){
    message("creating cache first time")
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
  }
  inv
}

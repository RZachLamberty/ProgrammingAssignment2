## cachematrix.R
##
## This R module contains two functions:
##
##  makeCacheMatrix - This function creates a matrix which has the ability to
##                    cache its inverse
##  cacheSolve      - The matrix which actually returns the cached inverse when
##                    available; otherwise calculates and cache it


## Create a list of functions which will be used to create and interact with a
## cache matrix, as well as the functions for calculating the inverse

makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## This function takes cachematrices (and possibly other arguments) and checks
## to see if the inverse has previously been computed. If so, it returns the
## cached inverse; if not it performs the calculation and caches the result.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}

# # This function is used to create a specail "matrix"
## object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  create <- function(y) {
    x <<- y
    inv <<- NULL
  }
  calc <- function() x
  createInverse <- function() inv <<-solve(x)
  calcInverse <- function() inv
  list(create = create, calc = calc, createInverse = createInverse, calcInverse = calcInverse)
}


## This function uses the inverse calculated from makeCacheMatrix. If the inverse was 
## was already found and the matrix hasn't changed then cachesolve will
## calculate the inverse using the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$calcInverse()
    if (!is.null(inv)) {
      message("cached data")
      return(inv)
    }
    matrix <- x$calc()
    inv <- solve(matrix, ...)
    x$createInverse(inv)
    inv
}

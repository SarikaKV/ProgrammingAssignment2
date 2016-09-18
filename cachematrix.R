## Author: Sarika Kondra
## Matrix inversion is usually a costly computation and it is benefitial to cache the inverse of a matrix rather than compute it repeatedly.
## The functions in this cacheMatrix.R file will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed, then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting from cached data")
    return(inv)
  }
  
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#Testing above code
myMatrix <- makeCacheMatrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))
myMatrix$get()
cacheSolve(myMatrix)
myMatrix$getinverse()
#Different matrix, inverse is again calculated and new cache is created
myMatrix$set(matrix(c(0, 2, 2, 0), nrow = 2, ncol = 2, byrow = TRUE) )
cacheSolve(myMatrix)

#myMatrix <- makeCacheMatrix(matrix(c(0, 2, 2, 0), nrow = 2, ncol = 2, byrow = TRUE))
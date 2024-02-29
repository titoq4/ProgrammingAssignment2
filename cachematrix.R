#script for ProgrammingAssignment2 (see comments below)
makeCacheMatrix <- function(x = matrix(rnorm(9), 3, 3)) {
  inverseCache <- NULL
  setMatrix <- function(matrix) {
    x <<- matrix
    inverseCache <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverseMatrix) inverseCache <<- inverseMatrix
  getInverse <- function() inverseCache
  list(set = setMatrix, get = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(cachedMatrix, ...) {
  inv <- cachedMatrix$getInverse()
  if (!is.null(inv)) {
    message("Retrieving cached inverse.")
    return(inv)
  }
  matrixData <- cachedMatrix$get()
  inv <- solve(matrixData, ...)
  cachedMatrix$setInverse(inv)
  inv
}
# These functions are used for caching the inverse of a matrix.
# 'makeCacheMatrix' creates a matrix and its inverse.
# It provides methods to set and get the matrix and its inverse.
# 'cacheSolve' computes the inverse of the matrix. If the inverse has already been computed (and cached), it retrieves the inverse from the cache instead
# of recomputing it, thereby saving time on repeated inversions of the same matrix.

# 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
# It accepts an optional matrix 'x' as input and returns a list of functions to interact with the matrix and its inverse. If no matrix is provided, it initializes
# an empty matrix.
makeCacheMatrix <- function(x = matrix()) {
    # intialize the inverse as NULL
    inv <- NULL

    #to set the matrix and reset the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    #to get the current matrix
    get <- function() x

    #to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse

    #to get the inverse, if it exists
    getInverse <- function() inv

    #return a list of methods for external use
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# 'cacheSolve' calculates the inverse of the "matrix" created by 'makeCacheMatrix'.
# If the inverse has already been calculated and cached, it returns this cached value.
# If not, it computes the inverse, caches it, and then returns the inverse.
# The function takes a 'cached matrix' object returned by 'makeCacheMatrix' and optional parameters for the 'solve' function.
cacheSolve <- function(x, ...) {
# Check if the inverse has already been calculated
inv <- x$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}
    
# Compute the inverse, cache it, and return it
    matrixData <- x$get()
    inv <- solve(matrixData, ...)
    x$setInverse(inv)
    inv
}

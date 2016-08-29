## This file contatins two functions
## 1. Create and cache inverse of a give matrix
## 2. Return the inverse of a cached matrix if exists, 
## otherwise create the inverse of the given matrix

## Below function creates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  setInverseMatrix <- function(y) {
    x <<- solve(y)
    m <<- NULL
  }
  getInverseMatrix <- function() x
  setCachedInverseMatrix <- function(mean) m <<- mean
  getCachedInverseMatrix <- function() m
  list(setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix,
       setCachedInverseMatrix = setCachedInverseMatrix,
       getCachedInverseMatrix = getCachedInverseMatrix)
  
}


## Returns the inverse of a matrix. 
## Creates an inverse if the matrix is not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getCachedInverseMatrix()
  if(!is.null(m)) {
    message("getting cached inverse of the given matrix")
    return(m)
  }
  data <- x$getInverseMatrix()
  m <- solve(x)
  x$setCachedInverseMatrix(m)
  m
}

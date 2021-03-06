## This file contatins two functions
## 1. Create and cache inverse of a give matrix
## 2. Return the inverse of a cached matrix if exists, 
## otherwise create the inverse of the given matrix

## Below function creates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setCachedInverseMatrix <- function(solvedMatrix) m <<- solvedMatrix
  getCachedInverseMatrix <- function() m
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setCachedInverseMatrix = setCachedInverseMatrix,
       getCachedInverseMatrix = getCachedInverseMatrix)
  
}


## Returns the inverse of a matrix. 
## Creates an inverse if the matrix is not already cached.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getCachedInverseMatrix()
  if(!is.null(m)) {
    message("getting cached inverse of the given matrix")
    return(m)
  }
  newMatrix <- x$getMatrix()
  inverseMatrix <- solve(newMatrix, ...)
  x$setCachedInverseMatrix(inverseMatrix)
  inverseMatrix
}

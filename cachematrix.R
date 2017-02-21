## Matrix inversion is expensive, the functions below enable the inverse
## matrix to be cached so that it's only calculated once.

## This function returns an object that stores and retrieves a matrix and its' 
## inverse. The return list has 4 functions:
##    - setMatrix() - sets the matrix (and resets the invers matrix)
##    - getMatrix() - returns the stored matrix
##    - setInverse() - sets the inverse matrix
##    - getInverse() - retrieves the inverse matrix

makeCacheMatrix <- function(mtrx = matrix()) {
  inv_matrix <- NULL
  setMatrix <- function(y) {
    mtrx <<- y
    inv_matrix <<- NULL
  }
  
  getMatrix <- function() mtrx
  setInverse <- function(inverse) inv_matrix <<- inverse
  getInverse <- function() inv_matrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse matrix 'lazily' (i.e. it will only
## calculate the inverse matrix the first time) and then caches the result so
## that it doesn't need to be calculated again

cacheSolve <- function(cache_matrix, ...) {
  inv_mtrx <- cache_matrix$getInverse()
  if(!is.null(inv_mtrx)) {
    message("getting cached inverse matrix")
  } 
  else 
  {
    message("calculating the inverse matrix")
    data <- cache_matrix$getMatrix()
    inv_mtrx <- solve(data, ...)
    cache_matrix$setInverse(inv_mtrx)  
  }
  
  inv_mtrx
}

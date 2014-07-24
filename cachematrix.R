##  This function returns a list containing 4 functions that are required to set and inverse a matrix and to
##  get the matrix and its inverse. It also holds the matrix and its inverse, if the inverse is alredy done.
##  If the inverse is not done, the cacheSolve function creates it and set it.

makeCacheMatrix <- function(mat = matrix()) {

  invMat <- NULL
  setMatrix <- function(inputMat) {
    mat <<- inputMat
    invMat <<- NULL  
  }
    getMatrix <- function() mat
    setInverse <- function(inv)  invMat <<- inv 
    getInverse <- function() invMat
    
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the output of makeChaceMatrix as input and returns the inverse of the 
## matrix that was passed to makeCacheMatrix. This function checks if the inverse of the matrix
## is present in the cache or not. If not, it calculates it by invoking "solve" and then sets 
## it as the inverse of the matrix in the cache, so that a invoke with the same data, returns the 
## result from the cache.

cacheSolve <- function(cacheData, ...) {
     
  ## First check if the inverse of the cached matrix is present in the cache
  invMat <- cacheData$getInverse()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  
  orgMat <- cacheData$getMatrix()
  invMat <- solve(orgMat, ...)
  cacheData$setInverse(invMat)
  invMat
  ## Return a matrix that is the inverse of the cached matrix
}

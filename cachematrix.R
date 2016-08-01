## makeCacheMatrix takes a matrix, then creates an object that cointains this matrix and can set and get its inverse. With this object as an argument, cacheSolve computes an inverse matrix if there's none cached, otherwise, it retrieves the cached inverse matrix and returns it.

## This function takes a matrix, imputMatrix, as an argument, creates and initializes cacheMatrix as a symbol that will be used to cache inverse matrix, and creates a list object that contains four functions.
## setMatrix  stores the imputMatrix for future retrieval. getMatrix returns the stored matrix. setInverse stores the inverse matrix of imputMatrix. getInverseMatrix returns the inverse matrix set with setInverseMatrix.
## This functions are named, so they can be accesed using  "$" in cacheSolve

makeCacheMatrix <- function(imputMatrix = matrix()) {
  cacheMatrix <- NULL
  setMatrix <- function(newImputMatrix) {
    imputMatrix <<- newImputMatrix
    cacheMatrix <<- NULL
  }
  getMatrix <- function() imputMatrix
  setInverseMatrix <- function(inverseMatrix) cacheMatrix <<- inverseMatrix
  getInverseMatrix <- function() cacheMatrix
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverseMatrix = setInverseMatrix,
    getInverseMatrix = getInverseMatrix
  )
}

## cacheSolve takes an object created with makeCacheMatrix. Then, if in that object there's already exists a stored inverse matrix in the symbol cacheMatrix, this is done using the function getInverseMatrix, as declared in makeCacheMatrix, called by "$getInverseMatrix". If this happens, a message telling us that a cached inverse has been retrieved is printed along with the inverse matrix stores in cacheMatrix.
## Otherwise, cacheSolve uses "$getMatrix" to retrieve our imputMatrix, then computes an inverse matrix using the function "solve" with its default parameters. The result is stores in cacheMatrix and stored using "$setInverseMatrix", overwriting cacheMatrix in the process. Finally, cacheMatrix is returned.

cacheSolve <- function(madebyCacheMatrix, ...) {
  cacheMatrix <- madebyCacheMatrix$getInverseMatrix()
  if(!is.null(cacheMatrix)) {
    message("Getting cached inverse of the matrix")
    return(cacheMatrix)
    }
  matrixToSolve <- madebyCacheMatrix$getMatrix()
  cacheMatrix <- solve(matrixToSolve)
  madebyCacheMatrix$setInverseMatrix(cacheMatrix)
  cacheMatrix
  }

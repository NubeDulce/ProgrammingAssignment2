## Take a matrix, cache it, then cache it's inverse
## for future use

## Create a list of functions to set a matrix, retrieve
## a matrix, set the inverse of the matrix, and retrieve
## the inverse of the matrix

makeCacheMatrix <- function(myMatrix = matrix()){
  
  ##clear out any cached inverse and cache the input
  ##matrix
  cachedInverseMatrix <- NULL
  cachedMatrix <- myMatrix
  
  ##create function setMatrix for caching a new 
  ##matrix and reset the inverse
  setMatrix <- function(newMatrix){
    cachedMatrix <<- newMatrix
    cachedInverseMatrix <<- NULL
  }
  
  ##create function getMatrix to print the currently cached 
  ##matrix 
  getMatrix <- function(){
    cachedMatrix
  }
  
  ##create function setInverse to cache the inverse 
  ##matrix calculated outside the function
  setInverse <- function(inverse){
    cachedInverseMatrix <<- inverse
  }
  
  ##create function getInverse to print the currently cached 
  ##matrix inverse
  getInverse <- function(){
    cachedInverseMatrix
  }
  
  #create the list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## a function that takes the calculates the inverse of
## a matrix if there is no cached value, or retrieves
## the cached value if available
cacheSolve <-function(matrixInput){
  
  matrixInputInverse <- matrixInput$getInverse() 
  
  if(!is.null(matrixInputInverse)){
    
    message("getting cached Inverse...")
    message("done")
    return(matrixInputInverse)
    
  }
  
  invertedMatrix <- solve(matrixInput$getMatrix())
  message("calculating and caching Inverse...")
  matrixInput$setInverse(invertedMatrix)
  message("done")
  return(invertedMatrix)
  
}

## I have written two functions to showcase the behaviour or lexical scoping and also the concept of caching.
## The underlying logic is, if there is any complex calculation in the program, the value of which may be needed frequently, cache it so that there is 
## no need to calculate again again.

## makeCacheMatrix function takes input matrix data and creats four sub functions
## setMatrix()  to capture the matrix data as input by user
## getMatrix() returns the value of matrix data
## setInv() to set the inverse of matrix 
## getInv() to return the inverse of matrix
## Finally, list is created to make above four function value available to outside usage

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inv) invMatrix <<- inv
  getInv <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function takes makeCacheMatrix() as input and checks if inverse of matrix is already calculated or not.
## If inverse is already calculated (in cache), it prompts appropriate message and returns the inverse value and function terminates.
## If inverse is not already calculated, it calculates the inverse, stores in cache, returns the inverse value and function terminates.

cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInv()
  if(!is.null(invMatrix)){
    message("Showing Cached Data")
    return(invMatrix)
  }
  dataMatrix <- x$getMatrix()
  invMatrix <- solve(dataMatrix, ...)
  x$setInv(invMatrix)
  invMatrix
}

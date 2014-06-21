## Program containing one functions that stores a matrix and its inverse, and has 
## functions for returning the inverse and taking another function to set the inverse
## The other function takes an object of the first function as a parameter and sets its inverse,
## if not already set.

## This function creates a matrix, which allows for cached values.
## The set function sets the value of a matrix to the variable 'x', and initializes the 'inverse' variable to NULL
## The get function returns the matrix 'x'
## The setinverse function allows for the 'inverse' to be set taking a function as a parameter which is used to set the 'inverse'
## The getinverse function returns the inverse of the matrix 'x', held in the variable 'inverse'

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a makeCacheMatrix and some parameters as arguments, if the function 
## already has an inverse then the message "getting cached data" is printed and the inverse
## is returned. Otherwise the matrix is taken from the input parameter 'x' using the function get, 
## then the inverse is calculated and passed to the setinverse function of the parameter 
## object. The last line returns the parameter object 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  x
}

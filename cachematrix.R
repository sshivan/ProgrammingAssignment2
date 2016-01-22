# "x"  function creates a special "matrix" object that can cache its inverse.
#
#  1. set the matrix
#  2. get the matrix
#  3. set the inverse
#  4. get the inverse
#  this list in the function is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  
  nl = NULL
  set = function(y) {
    # using `<<-` to assign a value to an object in an environment different from the current environment. 
    
    x <<- y
    nl <<- NULL
  }
  get = function() x
  setInverse = function(inverse) nl <<- inverse 
  getInverse = function() nl
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

## return: inverse of the original matrix input to makeCacheMatrix()


cacheSolve <- function(x, ...) {
  
  nl = x$getInverse()
  
  # it first checks to see if the inverse has already been calculated
  if (!is.null(nl)){
    # if so it gets the result from the cache and skips the computation. 
    message("getting cached data")
    return(nl)
  }
  
  data <- x$get()
  # otherwise, calculates the inverse 
  nl <- solve(data, ...)
  #inv = solve(matrix.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInverse(nl)
  
  nl
}

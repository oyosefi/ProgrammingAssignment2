## Put comments here that give an overall description of what your
## functions do
# We use two functions (surprise, surprise :-) )
# The first function, makeCacheMatrix, creates a list which acts as a "wrapper" object that contains
# the matrix, its inverted matrix (if solved), and functions to set/get each
#
# The second function gets such a wrapper object, return the inverted matrix if was calculated already
# or calculates it and stores the inverted matrix for future use

## Write a short comment describing this function
# The function makes use of the Lexical scoping rules to use the "sol" variable which is defined
# in the parent environment and stores the solution (i.e. inverted) matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # The object caching the inverted matrix (solution)
  sol <- NULL
  
  # setting a new matrix will invalidate the cached solution by setting it to NULL
  set <- function(y){
    x <<- y
    sol <<- NULL
  }
  
  # get the source matrix (i.e. the one to be inverted)
  get <- function() {
    x
  }
  
  # set the solution (inverted matrix) and store it in sol
  setinverted <- function(inverted) {
    message(sol)
    sol <<- inverted
  }
  
  # get the solution (which can also be NULL)
  getinverted <- function() {
    sol
  }
  
  # return a list of the 4 accessor functions
  list(set=set, get=get, setinverted=setinverted, getinverted=getinverted)

}


## Write a short comment describing this function
# The function checks if the matrix has already been inverted by calling x$getinverted()
# if it was, the cached solution is returned (saving calculation time)
# if there is no cached solution, we solve the matrix and cache the solution for 
# future use by calling x$setinverted()

cacheSolve <- function(x, ...) {
  # check the cache
  m <- x$getinverted()
  
  # return cached solution, if one exists
  if (!is.null(m))
  {
    message("returning cached solution")
    return(m)
  }
  
  # No cached solution, solve now
  data <- x$get()
  m <- solve(data, ...)
  
  # store in the cache
  x$setinverted(m)
  
  #and return the solution
  m
}

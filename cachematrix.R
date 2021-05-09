## the input x as a matrix
## and then set the solved value "sol" as a null
## then "mean" standard function to "solve" standard function
#1. set the value of the matrix 2. get the value of the matrix 
#3. set the value of inverse of the matrix 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) sol <<- solve
  getsolve <- function() sol
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##
## "mean" to "solve", "m" to "sol"
cacheSolve <- function(x, ...) {
  sol <- x$getsolve()
  if(!is.null(sol)) {
    message("getting inversed matrix")
    return(sol)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}



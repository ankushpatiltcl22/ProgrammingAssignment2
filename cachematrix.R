## The following functions calculate the inverse of a matrix and saves it
## This function creates a special "matrix" object 
## Stage1:- Initially set the value of the matrix
## Stage2:- get the value of the matrix
## Stage3:- set the value of the inverse
## Stage4:- get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## inverse of the "matrix" created using following function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

##test cases are present in Read_me.text file

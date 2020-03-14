## The following functions calculate the inverse of a matrix and saves it
## This function creates a special "matrix" object 
## Stage1:- Initially set the value of the matrix
## Stage2:- get the value of the matrix
## Stage3:- set the value of the inverse
## Stage4:- get the value of the inverse

## Mainly used variables in the functions are matrix,x,y

makeCacheMatrix <- function(x = matrix()) {
        
        ## define the cache matrix
        matrix <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the environment

                matrix <<- NULL ## re-initialize matrix in the environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) matrix <<- inverse ## set the cache matrix equal to the inverse of the matrix x

        getinverse <- function() matrix ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## inverse of the "matrix" created using following function


cacheSolve <- function(x, ...) {

        
        matrix <- x$getinverse()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data, ...)
        x$setinverse(matrix)
        matrix
}

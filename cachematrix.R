## The function "cacheSolve.R" returns the inverse of a matrix that is returned by the 
## function "makeCacheMatrix.R". This inverse can either be calculated newly or originate  
## from the cache if it was calculated earlier

## The input for the function is a square invertible matrix. The functions contains of functions
## to get/set the values of the matrix and to get/set the values of the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function takes as input a matrix x created by "makeCacheMatrix.R". It returns the
## inverse of the matrix x by first checking if the inverse is cached. If this is not the case
## the inverse is calculated and set. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

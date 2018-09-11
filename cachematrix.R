## The following set of functions will do the following:
## 1. Create a special matrix object which can store the 
##    inverse of a square invertible matrix
## 2. Calculate the Inverse if the inverse has not been calculated yet



## This function creates a special "matrix" object 
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL    ## Initiates inv as a null object
        
        set <- function(y) { ## Set the the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x ## Get the Matrix
        setinv <- function(inverseMatrix) inv <<- inverseMatrix ## Set Inverse 
        getinv <- function() inv ## Get Inverse
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve function below takes in a matrix and checks whether its
## inverse has been calculated. If yes, it returns the cached inverse and
## if not, it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ## Get inverse from makeCacheMatrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) ## Return the cached inverse if it exists
        }
        data <- x$get() ## Get the matrix which needs to be inverted
        inv <- Solve(data, ...) ## Calculates the inverse
        x$setinv(inv) ## Set the inverse Matrix
        inv ## return the Inverse Matrix
}

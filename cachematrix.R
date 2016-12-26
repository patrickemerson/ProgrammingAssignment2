## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will make a matrix object and cache its inverse
## This follows the same process MakeVector
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ##intialize now for use later in function
        set <- function(y) {  ##assign from parent env
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, ##use list to name elements for use outside of function
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## this function computes the inverse of the matrix from makeCacheMatrix
## this follows the format of cachemean
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
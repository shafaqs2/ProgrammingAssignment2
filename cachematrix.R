## Put comments here that give an overall description of what your
## functions do
## This function calculates the inverse of a matrix (provided that it has an inverse).
## First time around it computes the inverse of the matrix in question and stores it in a different environment
## i.e. it caches its value. The second time around when it nead the inverse of the same matrix, 
## then it simply gets the inverse form cache its value from the other environment instead of computing it from scratch.

## The following function creates a list that sets and gets the values of the matrix and its inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the matrix in question. it first checks if the inverse is already calculated.
## If it is calcualed, then it simply returns its value, else it calcualtes the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
## This returns inverse of the matrix submitted to makeCashematrix function.

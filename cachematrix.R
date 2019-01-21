## These functions together are used to calculate and cache the 
## inverse of a square invertible matrix. 

## The first function - makeCache Matrix - creates a special "matrix" 
## object that can cache its inverse.
## It does this by creating a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <- function() x
        setmatinv <- function(matinv) m <<- matinv
        getmatinv <- function() m
        list(setmat = setmat, getmat = getmat,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## The second function - cacheSolve - calculates the inverse of
## the matrix specifed when invoking makeCacheMattrix, after
## first checking to see if the inverse matrix has already
## been calculated. If it has, cacheSolve gets the inverse matrix 
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse
## matrix in the cache via the setmatinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmat()
        m <- solve(data, ...)
        x$setmatinv(m)
        m
}

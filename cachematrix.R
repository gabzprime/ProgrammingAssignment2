## Creates a special matrix that may contain 
## its inverse. 
## 
## x - The matrix that will be the base of the
## special matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the matrix if 
## the special matrix  created by makeCacheMatrix
## hasn't computed the inverse. If computed it will
## get from cache.
##
##  x  - The matrix created by makeCacheMatrix
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



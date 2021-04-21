## The following functions take an invertible matrix and cache the inverse
## to save time by avoiding repeated costly computations.

## makeCacheMatrix creates a special "matrix" object that can cache the inverse
## of a supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of the supplied matrix has already
## been computed. If so, cacheSolve retrieves the inverse from the cache. If
## not, cacheSole computes the inverse of the supplied matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cahced data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

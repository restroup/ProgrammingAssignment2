## Together, makeCacheMatrix() and cacheSolve() allow you
## store store a special matrix object that can cache its
## inverse, and then determine if the inverse matrix should
## be generated or used from cache.

## This function creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function checks to see if the inverse of the
## matrix is contained in cache. If it is, then the
## cached inverse is returned. If not then the inverse
## is generated, stored in cache, and then returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                ## message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

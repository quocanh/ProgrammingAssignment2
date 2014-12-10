## This code implements two functions to help caching matrix inversion

## This function creates a special matrix object from an input matrix
## This function should be used in combination with cacheSolve()
##   to cache the inversion of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvt <- function(invt) inv <<- invt
    getinvt <- function() inv
    list(set = set, get = get,
        setinvt = setinvt,
        getinvt = getinvt)
}


## When this function is called for the first time on a special matrix object
##   it calculates the inversion of the matrix and caches that.
## Subseqent call to this function with the same object the cached inversion
##   will be used without re-calculating it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvt()
    if(!is.null(inv)) {
        message("Found cached inversion, using it")
        return(inv)
    }
    m_data <- x$get()
    inv <- solve(m_data, ...)
    x$setinvt(inv)
    inv
}

## This document tries to cut down calculation time of solving a matrix by caching the calculation result in advance.

## This function is actually a list of functions trying to: 1.get & set a matrix 2.get & set the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function aims at solving a matrix but at first tests if the matrix has been calculated before.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
## Return a matrix that is the inverse of 'x'
}

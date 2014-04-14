## This script enables to cache results of intvering matrices, thus making
## repititive matrices inversion more efficient.

## Caches a given matrix
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


## Perform a matrix inversion;  in case it was already done (and cached), fetching the
## cached result.  Otherwise, performing the inversion and caching the result for next time.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x, ...)
    x$setinverse(inv)
    inv
}

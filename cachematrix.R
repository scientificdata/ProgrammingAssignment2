## This script enables to cache results of intvering matrices, thus making
## repititive matrices inversion more efficient.

## Caches a given matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # setter for the value of the matrix
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        
        # getter for the value of the matrix
        get <- function() x
        
        # setter for the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # getter for the inverse
        getinverse <- function() inv
        
        # return a list of all the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Perform a matrix inversion;  in case it was already done (and cached), fetching the
## cached result.  Otherwise, performing the inversion and caching the result for next time.
cacheSolve <- function(x, ...) {
    # checking if the inverse was cached already
    inv <- x$getinverse()
    
    # if so, return the cached version
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, calculate the inverse and cache it for next time
    data <- x$get()
    inv <- solve(x, ...)
    x$setinverse(inv)
    inv
}

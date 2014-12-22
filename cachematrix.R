## These two functions are for caching the inverse of a matrix. 

## Function returns a list of functions used to store a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setCachedInverse <- function(inverse) i <<- inverse
        getCachedInverse <- function() i 
        list(set = set, get = get,
             setCachedInverse = setCachedInverse,
             getCachedInverse = getCachedInverse)
}


## Function that solves the inverse of a matrix and caches the inverse in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getCachedInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setCachedInverse(i)
        i
}

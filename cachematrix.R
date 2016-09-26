## Matrix inversion is a costly computation and can
## benefit from caching. Following functions do that.

## Function that creates a "special" matrix that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function that computes the inverse of the
## "special" matrix if not cached yet and
## returns cached result otherwise

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i);
    }
    data <- x$get()
    i <- solve(x)
    x$setinverse(i)
    i
}

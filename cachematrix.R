## An exercise in understanding lexical scoping.

## This is a special matrix object.
## Adds the matrix and its inverse to the global context, i.e. caches the matrix and its inverse

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


## This function takes a makeCacheMatrix object and returns its inverse.
## It checks to see whether the inverse has already been calculated.
## If it finds the inverse in the global context, it returns it; otherwise it calculates it
## and calls makeCacheMatrix to cache the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

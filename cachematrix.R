## The two functions written below work as a pair and cache the inverse of a matrix. 
## Matrix inversion is computationally expensive process. It would be better to cache already computed values
## so that it can be looked up in the cache when we need it again rather than re-computing the same.

## The first function, makeCacheMatrix, creates a spectial "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

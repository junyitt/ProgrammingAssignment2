## Let x be the input invertible matrix, 
## makeCacheMatrix(x) will return a list of four functions which cache the inverse of x if the inverse was previously computed.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function () m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Let y be assigned makeCacheMatrix(x), i.e y <- makeCacheMatrix(x)
## if the inverse of x was previously cached into the variable m in the makeCacheMatrix() environment; 
## cacheSolve(y) will then return the inverse of x;
## else, the inverse of x will be computed and printed.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
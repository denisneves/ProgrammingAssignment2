## Functions' purpose is to output the inverse of an invertible square matrix input and keep that in cache after the second
## execution.

## This function has the getters and setters to keep the inverse of the square matrix information, interating with the external
## environment and outputing functions as a list.

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function solves the inversible matrix in its first run and get the result from cache after the second one.

cacheSolve <- function(x, ...) {
s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(matrix(data = data, nrow = 2, ncol = 2))
    x$setsolve(s)
    s        ## Return a matrix that is the inverse of 'x'
}

## Below are two functions that are used to create 
## a special object that stores a matrix
## and caches its inverse.


## This function creates a special "matrix" object that
# can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setim <- function(solve) im <<- solve
        getim <- function() im
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix hasn't 
## changed), then cacheSolve should return the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        im <- x$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setim(im)
        im
        ## Return a matrix that is the inverse of 'x'
}

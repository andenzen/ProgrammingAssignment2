## Author: Antti Merisalo
## Date created: 2018-07-19

## makeCacheMatrix function creates a matrix object that can cache also 
## its inverse inside 'inv' variable

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function solves the matrix created before in terms of identity
## matrix, i.e. it returns its inverse. If the inverse has already solved before
## the function does not recalculate it but instead fetches it from the cache of
## the beforecreated matrix object.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("obtaining inverse from the cache")
        return(inv)
    }
    
    temp <-x$get()
    inv <- solve(temp)
    x$setinv(inv)
    return(inv)
}

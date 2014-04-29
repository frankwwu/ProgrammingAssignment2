## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inversed x ‘inv_x’ null
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    ## Set the inversed x
    setinverse<- function(inverse) inv_x <<-inverse
    ## Get the inversed x
    getinverse <- function() inv_x
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## If the inversed x was cached already, return it.
## If the inversed x was not cached, calculate it, and return it.
cacheSolve <- function(x, ...) {
    ## Get the value of inversed x
    inv_x <- x$getinverse()
    ## If the inversed x was cached already, return it.
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    ## If the inversed x was not cached, calculate and return it.
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    ## Return a matrix that is the inverse of 'x'
    return(inv_x)
}

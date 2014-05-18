## Programming Assignment 2
## These functions can be used to cache the results of a matrix inverse operation.
## The matrix is assumed to be invertible.

## The makeCacheMatrix function is used to create a cacheable matrix.
## The argument x is the matrix for which we want to find the inverse.

## The local variable minv is used to store the value of the matrix inverse
## once it has been calculated.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(invr) minv <<- invr
    getinverse <- function() minv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function finds the inverse of the matrix that is passed in.
## If the inverse has already been computed previously, the cached value is
## returned.
## Otherwise, the inverse is computed using solve() and stored in the cache
## for future use if it is needed again.

## If x contains an invertible matrix, an example of the usage would be:
## xc <- makeCacheMatrix(x)
## cacheSolve(xc)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv);
    }
    ## No cached value available. Need to compute the inverse
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
}

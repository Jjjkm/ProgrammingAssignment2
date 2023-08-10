## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ma <- NULL
    set <- function(y) {
        x <<- y
        ma <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(inverse) ma <<- inverse
    getinvmatrix <- function() ma
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ma <- x$getinvmatrix()
    if(!is.null(ma)) {
        message("getting cached data")
        return(ma)
    }
    data <- x$get()
    ma <- solve(data, ...)
    x$setinvmatrix(ma)
    ma
    
}

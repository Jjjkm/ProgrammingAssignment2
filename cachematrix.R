## 2 functions cache the inverse of a matrix

## The 1st function creates a matrix object and cache its inverse

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


## The 2nd function inverses the matrix provided above and cache it.

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

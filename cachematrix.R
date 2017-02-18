## caculate and cache inverse of a matrix
##

## make a list containing functions for setting and getting inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        invse <- NULL
        set <- function(y) {
                x <<- y
                invse <<- NULL
        }
        get <- function() x
        setinvse <- function(solve) invse <<- solve
        getinvse <- function() invse
        list(set = set, get = get,
             setinvse = setinvse,
             getinvse = getinvse)
}


## check if there is pre-existing inverse and if so get it; otherwise calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invse <- x$getinvse()
        if(!is.null(invse)) {
                message("getting cached data")
                return(invse)
        }
        data <- x$get()
        invse <- solve(data, ...)
        x$setinvse(invse)
        invse
}

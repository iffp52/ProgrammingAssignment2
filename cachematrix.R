## This function creates a special "matrix" object that can cache its inverse.
## Set the value of the matrix
## Get the value of the matrix
## Set the inverse of the matrix
## Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inversem <- NULL
        set <- function(y) {
                x <<- y
                inversem <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inversem <<- inverse
        getinv <- function() inversem
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversem <- x$getinv()
        if(!is.null(inversem)) {
                message("getting cached data")
                return(inversem)
        }
        data <- x$get()
        inversem <- solve(data, ...)
        x$setinv(inversem)
        inversem
}

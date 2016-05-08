## First function creates matrix that can cache its
## inverse. Second function creates inverse matrix
## to cached matrix. There is no check if the matrix
## is inversible.

## This function creates matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        # Functions to set new and to get cached matrix.
        set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
        }
        get <- function() x
        ## Functions to set and get inverse matrix of the cached
        ## matrix.
        setInverse <- function(iMatrix) inverseMatrix <<- iMatrix
        getInverse <- function() inverseMatrix
        ## Return list of functions.
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## This function first checks if the inverse matrix
## is already set. If it is it returns cached value, 
## otherwise it makes inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        ## Check if inverse matrix was already set.
        if(!is.null(inverseMatrix)) {
                message("Getting cached data")
                return(inverseMatrix)
        }
        ## get cached matrix.
        data <- x$get()
        ## make inversion
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}

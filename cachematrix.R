## This function creates a matrix and caches its inverse 

## Creates the matrix and an object representing the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Checks if the matrix inverse is calculated, if not, then calculate it

cacheSolve <- function(x, ...) {

        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}

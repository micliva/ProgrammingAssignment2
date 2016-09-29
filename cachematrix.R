## Below are two functions that are used to create a special object "matrix" 
## that stores a matrix and cache's its inverse. 
## In "Object-oriented" sleng we create object "matrix" with four methods.

## Creating object "matrix" with 4 methods

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Inverse "matrix" and save, or get it, if there is saving data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, diag(nrow(data)), ...)
        x$setinv(inv)
        inv
}

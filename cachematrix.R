## Put comments here that give an overall description of what your
## functions do
## 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # makeCacheMatrix takes a matrix as input and caches it and its inverse, 
        # returning as output a list with four functions
        # that can be used to set or get either the matrix itself or its inverse.
        # If the inverse has not been set before, it returns NULL.
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## cacheSolve returns a matrix that is the inverse of 'x', its input. 
        ## The input must be a list with the functions get, set, getinv and 
        ## setinv, which are returned by makeCacheMatrix. If the inverse had 
        ## been computed before, its cached object is retrieved using getinv(). 
        ## Otherwise, it is computed and cached using setinv().
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

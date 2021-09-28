## These functions compute the inverse (i) of a supplied matrix (X).
## Using the benefits of lexical scoping the functions and variables defined in 
## the make function are made available in the global environment
## To save processing time, the inverse is stored in cache and if the value of
## x has not changed then then the cached value is returned.
##



## Each instance of a makeCacheMatrix object based on the supplied matrix (x) is 
## created with a set of variables (x and i) and set and get functions for each
## of the matrices.
## Using the <<- assignment ensures the variables and their functions are
## available in the global workspace

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinvn = setinv,
             getinv = getinv)
}

## cacheSolve takes a makeCacheMatrix object and determines whether there is 
## already a value for the inverse stored in cache.  If there is it returns 
## this value with an appropriate message.  If not it calculates the inverse of 
## x, stores it in cache and returns its value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

#
# CACHING THE INVERSE OF A MATRIX
#

#-----------------------------------------------------------------------------------------
# FUNCTION makeCacheMatrix
# Creates a special "matrix", which is really a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the solve
# - get the value of the solve
#-----------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # - set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # - set the value of the matrix
        get <- function() x
        # - set the value of the inverse
        setsolve <- function(solve) m <<- solve
        # - get the value of the inverse
        getsolve <- function() m
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



#-----------------------------------------------------------------------------------------
# FUNCTION cacheSolve
# Return a matrix that is the solve of 'x'
#-----------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        # first checks to see if the solve has already been calculated
        m <- x$getsolve()
        # If so, it gets the solve from the cache and skips the computation
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise, it calculates the solve of the data and sets the value of the solve in the cache via the setsolve function
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

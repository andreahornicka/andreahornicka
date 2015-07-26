
# --------------------------------------------------------------------------
# 1. makeCacheMatrix 
# This function creates a special "matrix" object that can cache its inverse
# --------------------------------------------------------------------------

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    
 # Set the matrix
    setMatrix <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
 # Get the matrix
    getMatrix <- function() {
        m
    }
    
 # Set the Inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
 # Get the Inverse of the matrix
    getInverse <- function() {
        i
    }
    
 # List of all methods
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

# --------------------------------------------------------------------------
# 2.cacheSolve
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache
# --------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    # Matrix that is the inverse of 'x'
    m <- x$getInverse()
    
 # Return the inversion
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
 # Get the matrix from the object
    data <- x$getMatrix()
    
 # Inversion of a square matrix
    m <- solve(data)
    
 # Set the inverse to the object
    x$setInverse(m)
    
 # Return the matrix
    m
} 

# --------------------------------------------------------------------------
# Validation
# --------------------------------------------------------------------------

a <- makeCacheMatrix( matrix(1:4, nrow = 2, ncol = 2) );
summary(a)

a$getMatrix();
cacheSolve(a)
a$getInverse()

cacheSolve(a)

# --------------------------------------------------------------------------

## The purpose is writing two functions makeCacheMatrix() and 
## cacheSolve() to compute inverse of a matrix using lexical scoping

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
## <<- operator is used to assign a value to an object in an
## environment that is different from the current environment.      
         x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setInverse <- function(solve,x) inv <- solve(x)
    getInverse <- function() inv
    list(setmatrix = setmatrix,
         getmatrix = getmatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    m <- x$getmatrix()
    inv <- solve(m)
    x$setInverse(m)
    inv
}


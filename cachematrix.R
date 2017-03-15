# FUNCTION:   makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# based on Cursera Assignmnet-2 code.R
makeCacheMatrix <- function(x = matrix()) {
    myInv <- NULL
    set <- function(y) {
           x <<- y
           myInv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) myInv <<- solve
    getInv <- function() myInv
    list(set = set, 
	     get = get,
         setInv = setInv,
         getInv = getInv)
}

# FUNCTION:   cacheSolve
# This function computes the inverse of the matrix returned by the function "makeCacheMatrix". If the Inverse has already been calculated, then the cacheSolve retrieves the Inverse from the cache.
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    myInv <- x$getInv()
    if(!is.null(myInv)) {
    message("Getting Cached Matrix")
    return(myInv)
    }
    data <- x$get()
    myInv <- solve(data, ...)
    x$setInv(myInv)
    myInv
}

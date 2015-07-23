## makeCacheMatrix creates a cacheMatrix object which stores a matrix
## together with functions to set and return the data in the
## matrix, and functions to set and get the matrix inverse.
## The value of the matrix inverse is cached once computed,
## until the value of the matrix is changed. 
##
## makeCacheMatrix(x = matrix())
##     returns a cacheMatrix object with the associated functions
##     documented immediately below. If x is not passed in, the
##     stored matrix is NULL.
##
## The cacheMatrix object is implemented as a list, and contains
## both data structures and functions (methods).
##
## Assume the matrix object is created with the call 
##     x <- makeCacheMatrix() 
## Then,
##
## x$get()  returns the value of the matrix stored by the object x.
##
## x$set(y)  stores the matrix y in the cacheMatrix object, x. 
##     set clears the cached inverse. The return value of set() is y. 
##
## cacheSolve(x) computes, caches and returns the value of the 
##     cacheMatrix object x, if x is invertible. 
##
##->>NOTE WELL: The function cacheSolve(x) above is the recommended
##     way to get and set the value of the inverse of x. All of the
##     compute, store, and retrieve tasks are accomplished by calling
##     cachesolve(x).
##
##-------------------------------------------------------------------
##->> THE FOLLOWING FUNCTIONS ARE INTENDED FOR INTERNAL USE ONLY <<--
##
##    They are not recommended for use by users of a CacheMatrix.
##    They are documented here to aid in code maintenance only.
##-------------------------------------------------------------------
##
## x$getInverse() returns the inverse of the stored matrix, if it is
##     invertible). If x is not invertible, this function returns NULL.
##
## x$setInverse(y) stores the value of the inverse, y, which must
##     be passed into the setInverse function. This function is for
##     internal use by cacheSolve(x).
##-------------------------------------------------------------------


makeCacheMatrix <- function(x = matrix()) {
    theInverse <- NULL
    set <- function(y) {

      ## set/reset the inverse to be NULL when the matrix is changed/created.
      ## The '<<-" operator accesses the global variables 'theInverse' and 'x'.
        theInverse <<- NULL 
        x <<- y
    }
    get <- function() x  ## returns the stored value of the matrix.
    
    setInverse <- function(tempInverse) theInverse <<- tempInverse
    getInverse <- function() theInverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## y <- cacheSolve(x) computes the inverse of the cacheMatrix
##     object stored in x. If the matrix is not invertible, an
##     error will be thrown by the solve() function, which
##     this function calls.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x', and then
    ## cache its value with x.
    
    theInverse <- x$getInverse()
    if(!is.null(theInverse)) 
    {
        message("getting cached inverse") 
        return(theInverse)
    }
    theMatrixData <- x$get()
    tempInverse <- solve(theMatrixData)
    x$setInverse(tempInverse)
    tempInverse
}

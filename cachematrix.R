## makeCacheMatrix creates an object that references a Matrix
## together with functions to set and return the data in the
## matrix. The object also contains functions to set and get
## the value of the matrix inverse, which is cached once computed,
## until the value of the matrix is changed. 
##
## makeCacheMatrix(x = matrix())
##     returns an empty matrix object with the associated functions
##     documented immediately below. 
##
## Assume the matrix object is created with the call 
##     x <- makeCacheMatrix()
## Then,
##
## x$get()  returns the value of the matrix stored by the object
##
## x$set()  returns the matrix stored by the object
##
## x$getInverse() returns the inverse of the stored matrix (assuming
##     it is invertible).
##
## cacheSolve(x) computes, caches and returns the value of the 
##     cacheMatrix object x.
##
## x$setInverse(y) stores the value of the inverse, y, which must
##     be passed into the setInverse function. This function is for
##     internal use by cacheSolve(x).
##
##     Note well: The function cacheSolve(x) above is the recommended
##     way to compute and set the value of the inverse of x. Both
##     tasks (compute and store) are accomplished by calling
##     cachesolve(x).
##
##  The cacheMatrix object is implemented as a list.

makeCacheMatrix <- function(x = matrix()) {
    theInverse <- NULL
    set <- function(y) {
      x <<- y
      theInverse <<- NULL
    }
    get <- function() x
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

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', and then
  ## cache its value with x.
  theInverse <- x$getInverse()
  if(!is.null(theInverse)) {
    message("getting cached inverse")
    return(theInverse)
  }
  theMatrixData <- x$get()
  tempInverse <- solve(theMatrixData)
  x$setInverse(tempInverse)
  tempInverse
}

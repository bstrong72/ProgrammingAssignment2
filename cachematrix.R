## this set of functions allows the user to create a "cache matrix" object,
## which can store the value of a matrix's inverse for later use, saving the
## user from having to use the solve() function to compute inverse every time
## it's needed.

## This function can be called to store a "cache matrix" object in a variable
## Requires a square, invertable matrix as a parameter to work properly

makeCacheMatrix <- function(x = matrix()) {
        # Set a variable representing the inverse of the passed matrix
        invrs <- NULL
        
        # allow matrix parameter to be changed
        set <- function(y) {
          x <<- y
          invrs <<- NULL
        }
        
        # get the matrix x
        get <- function() x
        
        # set and get the inverse of matrix x
        setinvrs <- function(n) invrs <<- n
        getinvrs <- function() invrs
        
        # return a list of previously defined functions. This is the "cache
        # matrix" object.
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## gets the inverse of a matrix that is stored in the form of the returned 
## object from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
        # Get the stored inverse of the matrix
        invm <- x$getinvrs
        
        # If inverse is defined return it
        if(!is.null(invm)) {
          message("getting cached data")
          return(invm)
        }
        
        # If not defined, run solve() to get it, set the object's invrs variable,
        # and return the inverse matrix
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvrs(invm)
        invm
}

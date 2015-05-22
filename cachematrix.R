## This pair of functions inverts a given matrix. This is performed by 
## cacheSolve(). cacheSolve() first checks to see if there is a cached value
## for the matrix inverse, and if so returns this value rather than
## recalculating it. It does this by calling the get_inverse() function defined
## in makeCacheMatrix(). If this variable is empty, cacheSolve() retrieves the
## (uninverted) matrix from makeCacheMatrix() via the get() function, inverts
## it, and passes the value back to makeCacheMatrix() for caching via
## set_inverse(). It then returns the inverted matrix.

## The function below takes one matrix as an argument and stores it's value. It
## outputs a list of four functions which can be called individually from
## cacheSolve(). These functions store the value of a matrix, return the
## matrix, store the value of the matrix's inverse, and return the inverse,
## respectively.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             # initialise inv
    set <- function(y) {    # set and store value of matrix to use
        x <<- y
        inv <<- NULL        # reset inv to NULL
    }
    get <- function() x     # return current matrix value
    set_inverse <- function(inverse) inv <<- inverse  # store inverse
                                                      # calculated in 
                                                      # cacheSolve()
    get_inverse <- function() inv    # returns stored value of inverse.
    list(set = set, get = get,       # return a list of all functions defined
         set_inverse = set_inverse,  # in makeCacheMatrix
         get_inverse = get_inverse)
}


## This function performs the calculation of the matrix inverse via the solve()
## function. However it first checks to see if there is a cached inverse value
## stored in the makeCacheMatrix function, and if so, outputs this value rather
## than computing the inverse anew.

cacheSolve <- function(x, ...) {  # takes an object with the value 
                                  # makeCacheMatrix, eg l <- makeCacheMatrix()
    
    inv <- x$get_inverse()        # get value of inverse via get_inverse()
    if(!is.null(inv)) {           # check if a value stored in 'inv' already
        message("getting cached data")
        return(inv)               # if so, retrieve and return this value and
                                  # exit the function 
    }

    ## code to be executed if inv is empty (proceed with inverse computation):
    data <- x$get()           # get value of matrix from makeCacheMatrix
    inv <- solve(data, ...)   # compute inverse
    x$set_inverse(inv)        # pass the result to makeCacheMatrix for caching
    inv                       # return result
}

## Two functions that calculate and cache the inverse of a matrix.
## It is assumed that the supplied matrix is invertable.

## Creates a matrix function object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    #varible to store cached inverse matrix
    my_inv <- NULL 
    # Set function for the matrix x
    f_set <- function(y) { 
        x <<- y
        my_inv <<- NULL
    }
    # Get function for the matrix x
    f_get <- function() x 
    # Set function for the inverse matrix
    f_set_inv <- function(inverse) my_inv <<- inverse 
    # Get function for the inverse matrix
    f_get_inv <- function() my_inv
    # Return the matrix via above functions
    list(f_set=f_set, f_get=f_get, f_set_inv=f_set_inv, f_get_inv= f_get_inv)
}

## computes the inverse of the above matrix function object
## and will create a cache of the result
## if a cache already exists, the inverse will not be recalculated
cacheSolve <- function(x, ...) {
    #get inverse of matrix x
    my_inv <- x$f_get_inv()
    # return the cached matrix, if available
    if (!is.null(my_inv)) {
        message("Retrieving cached inverse matrix")
    }
    # otherwise, calculate the inverse
    else {
        message("Caching inverse matrix")
        my_data <- x$f_get()
        my_inv <- solve(my_data, ...)
    }
    # cache the inverse
    x$f_set_inv(my_inv)
    # return inverse of matrix x
    return(my_inv)
}

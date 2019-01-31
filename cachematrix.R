## Put comments here that give an overall description of what your
## functions do

## Create an object with 4 methods and 1 data variable (matrix):
## set: change the data object (and reset inverse_m to NULL)
## get: return the data object
## get_inverse: return cached inverted matrix
## set_inverse: cache inverted matrix

makeCacheMatrix <- function(x = numeric()) {
    inverse_m <- NULL 
    set <- function(y) { # accept a new matrix from user and force calculation on next cacheSolve call
        x <<- y
        inverse_m <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inverse_m <<- inverse
    get_inverse <- function() inverse_m
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}

## cacheSolve will accept a makeCacheMatrix object as its argument.
## First, it will check if an inverse already has been cached via get_inverse call.
## If the returned value is not null, then cacheSolve will return the inverse returned from get_inverse.
## If get_inverse returns a null, then it will call get() for the matrix, run solve on it, and
## cache the result using the set_inverse call. Finally, it will return the the calculated inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_m <- x$get_inverse() 
    if (!is.null(inverse_m)) {
        message('getting cached data')
        return(inverse_m)
    }
    data <- x$get()
    inverse_m <- solve(data)
    x$set_inverse(inverse_m)
    inverse_m
}

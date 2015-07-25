## These functions are used for caching the inverse
## of a matrix in a special matrix object.
## Before solving to invert a matrix, the cache is
## checked to see if this has already been performed.


## This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set_matrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(solve) inverse <<- solve
    get_inverse <- function() inverse
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$get_inverse()
    if(!is.null(matrix_inverse)) {
        message("getting cached inverted matrix")
        return(matrix_inverse)
    }
    matrix_data <- x$get_matrix()
    ## Computing the inverse of a square matrix 
    ## can be done with the solve function in R.
    matrix_inverse <- solve(matrix_data, ...)
    x$set_inverse(matrix_inverse)
    matrix_inverse
}

## Example of successful output
# > source("cachematrix.R")
# > mat <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# > cache <- makeCacheMatrix(mat)
# > cache$get_matrix()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cache$get_inverse()
# NULL
# > cacheSolve(cache)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cache$get_inverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cache)
# getting cached inverted matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
## Assignment: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## Assumption: For this assignment, assume that the matrix supplied is always invertible.
###################################################################################
##
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
###################################################################################
makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_m <<- inv
        getinv <- function() inv_m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##################################################################################
##
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
##################################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinv()
        if(!is.null(inv_m)) {
                message("getting cached data")
                return(inv_m)
        }
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinv(inv_m)
        inv_m
}

#######################################Execution##################################

##> source("cachematrix.R")
##> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##> my_matrix$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> my_matrix$getinv()
##NULL
##> cacheSolve(my_matrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> my_matrix$getinv()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



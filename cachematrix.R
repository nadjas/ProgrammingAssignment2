## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_mtx <- NULL
        set_mtx <- function(y) {
                inv_mtx <<- NULL
                x <<- y
        }
        
        set_inv_mtx <- function(im) inv_mtx <<- im
        get_inv_mtx <- function () inv_mtx
        get_mtx <- function () x
        list (set_mtx = set_mtx, set_inv_mtx = set_inv_mtx, 
              get_inv_mtx = get_inv_mtx, get_mtx = get_mtx)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mtx <- x$get_inv_mtx()
        if (!is.null(inv_mtx)) {
                message("getting cached data")
                return(inv_mtx)
        }
        data <- x$get_mtx()
        inv_mtx <- solve(data)
        x$set_inv_mtx(inv_mtx)
        inv_mtx
}

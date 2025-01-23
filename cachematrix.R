## Put comments here that give an overall description of what your
## functions do


#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- cacheMatrix$getInverse()

        if (!is.null(inv)) {
                message("Getting cached inverse.")
                return(inv)
        }

        mat <- cacheMatrix$get()
        inv <- solve(mat)


        cacheMatrix$setInverse(inv)

        inv
}
## Create a wrapper for calculating the matrix inverse
## so it is only calculated when required - i.e. once
## it has first been calculated it is then saved for repeat
## access in case needed.

## NOTE - No validation has been inserted to allow
## for non-invertible matrices, so this wrapper
## will fail if non-invetible data are supplied.

## Create a cacheMatrix object.
## This is a list of four member functions,
## 1 set, ie. set matrix value,
## 2 get, ie. return matrix value, 
## 3 setinv, ie. set the inverse of the matrix,
## 4 getinv, ie. return the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Use the CacheMatrix object (list of functions) 
## defined by the makeCacheMatrix in order to efficiently
## solve the inverse of a matrix which may be required
## repeatedly. Each time cacheSolve is called the function
## checks whether the inverse needs to be recalculated.
cacheSolve <- function(x, ...) {
    ## Retrieve the cached inverse
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse from cache")
        return(inv)
    }
    ## Cached inverse is NULL so 
    ## it needs to be recalculated
    invertible_matrix <- x$get()
    inv <- solve(invertible_matrix)
    x$setinv(inv)
    inv
}

## The purpose of both the functions is to allow user to inverse the
## matrix and retrieve using cache memory to save time for the created 
## object

## function makeCacheMatrix helps user in creating object which can make 
## use of the sub-functions & instantiated/initialized arguments from 
## makeCacheMatrix's environment 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}
## function cacheSolve once called for the object created using 
## makeCacheMatrix stores the inverse of matrix in cache memory saving  of 
## lots computation time
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mymatrix <- x$get()
    inv <- solve(mymatrix, ...)
    x$setinv(inv)
    inv
}

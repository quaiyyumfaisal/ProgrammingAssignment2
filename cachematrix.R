## Put comments here that give an overall description of what your
## functions do

## There are two functions and they are makeCacheMatrix and cacheSolve
##makeCacheMatrix consists of set, get, setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   ##initializing inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x ##function used for getting matrix x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function is used to get the cache data

cacheSolve <- function(x, ...) ##Gets cache data
    {
    inv <- x$getinverse()
    if(!is.null(inv)) {                 ##checking whether inverse of the matrix is NULL
        message("getting cached data")  ##This sentence will be seen if we get the inverse of the from cache data
        return(inv)                     ##returns inverse value 
    }
    mat <- x$get() 
    inv <- solve(mat, ...)              ##calculates inverse value
    x$setinverse(inv)
    inv                                 ##Return a matrix that is the inverse of a squared matrix 'x'
}

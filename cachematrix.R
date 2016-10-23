##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## variable 'inv' is uesd to store the inverse of a matrix.
##The function 'makeCacheMatrix', creates a special "vector", which is really a list containing a function to
##set:set the value of the matrix
##get:get the value of the matrix
##setinverse:set the value of the inverse of a matrix
##getinverse:get the value of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieve the inverse from the cache
##Suppose X is a square invertible matrix, otherwise an error will occour.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

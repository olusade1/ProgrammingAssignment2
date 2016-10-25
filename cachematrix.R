## This pair of functions calculate and cache the inverse of a square matrix.

##  makeCacheMatrix: This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        setmat<- function (y){
                x <<- y
                inv <<- NULL
        }
        getmat<- function() x
        
        setinv<- function(solve) inv<<- solve
        getinv<- function() inv
        list(set = setmat, get = getmat, setinv = setinv, getinv = getinv)
        
}


## cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse has already been calculated for an unchanged matrix, then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
## Return a matrix that is the inverse of 'x'
}

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    i <- NULL
    set <- function(y) {
        x <<- y 
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    # If the inverse has already been calculated (and the matrix has not changed), 
    # then the cachesolve should retrieve the inverse from the cache.
    i <- x$getinverse() 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

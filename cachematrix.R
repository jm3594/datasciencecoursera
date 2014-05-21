## makeCacheMatrix and cacheSolve create a "matrix"
## like list that calculates the inverse of a matrix
## and stores it in the cache.


## makeCacheMatrix creates a list containing functions
## to set a matrix, get a matrix, store the inverse of
## a matrix in the cache, and get the inverse of the
## stored matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    	get <- function() x
    	setCache <- function(inverse) inv <<- inverse
    	getCache <- function() inv
    	list(set = set, get = get,
	setCache = setCache,
	getCache = getCache)
}


## cacheSolve calculates the inverse of a matrix and
## stores it in the cache. If the inverse is already
## stored in the cache, the function gets the inverse
## from the cache and skips the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'	
        inv <- x$getCache()
    	if(!is.null(inv)) {
		message("getting cached data")
        	return(inv)
    	}
    	data <- x$get()
    	inv <- solve(data, ...)
    	x$setCache(inv)
    	inv
}

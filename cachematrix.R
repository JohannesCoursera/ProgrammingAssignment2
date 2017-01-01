## Caching the Inverse of a Matrix:
## Instead of computing the inverse of a matrix repeatedly, it is more beneficial to cache the inverse of a matrix since the computation of a matrix is usually a costly computation

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL 
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## The following function calculates the inverse of the special matrix created by the above function. However, it firs checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the minverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


## This function computes the inverse of the matrix and stores the result in cache.

## This is the function to cache the matrix.

makeCacheMatrix <- function(x = matrix()) {
	# Variable to stores the cached inverse matrix
	cacheInverse <- NULL 
	
	# Function to Set/Initialize the matrix
	setMatrix <- function(y) { 
			x <<- y
			cacheInverse <<- NULL
	}
	
	# This function returns the stored matrix.
	getMatrix <- function() {
		x
	}
	
	# Cache the inverse of the matrix
	setInverse <- function(inverse) { 
		cacheInverse <<- inverse
	}
	
	# Get the cached value.
	getInverse <- function() {
		cacheInverse
	}
	
	# Returns a list of all variables with its values.
	list(setMatrix = setMatrix, getMatrix = getMatrix,
		 setInverse = setInverse,
		 getInverse = getInverse)

}


## The following function calculates the inverse of a "special" matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	# Getting the cached value. If found then it returns it.
	cacheInverse <- x$getInverse()
	if(!is.null(cacheInverse)) {
			message("getting cached data")
			return(cacheInverse)
	}
	
	# Else it computes the inverse of the matrix and returns the result.
	data <- x$getMatrix()
	cacheInverse <- solve(data, ...)
	x$setInverse(cacheInverse)
	
	cacheInverse
}

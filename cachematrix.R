## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	cacheInverse <- NULL # Variable to stores the cached inverse matrix
		
        setMatrix <- function(y) { # Function to Set/Initialize the matrix
                x <<- y
                cacheInverse <<- NULL
        }
        
		getMatrix <- function() {
			x
		}
        
		setInverse <- function(inverse) { 
			cacheInverse <<- inverse
        }
		
		getInverse <- function() {
			cacheInverse
        }
		
		list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		cacheInverse <- x$getInverse()
        if(!is.null(cacheInverse)) {
                message("getting cached data")
                return(cacheInverse)
        }
        data <- x$getMatrix()
        cacheInverse <- solve(data, ...)
        x$setInverse(cacheInverse)
        
		cacheInverse
}

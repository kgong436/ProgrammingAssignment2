## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix
## get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) i <<- solve
		getinverse <- function() i
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## Write a short comment describing this function
## Accepts a special matrix as a argument
## Computes the inverse if it is not cached
## Otherwise, return cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

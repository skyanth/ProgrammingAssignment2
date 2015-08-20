# Since calculating the inverse of a matrix can be quite an expensive computation,
# these functions are used to a. create a special object that stores a matrix and caches its inverse,
# and b. get the inverse from the cache (or calculate and cache it if it is not cached).

# makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
	    set <- function(y) {
	            x <<- y
	            i <<- NULL
	    }
	    get <- function() x
	    setinverse <- function(inverse) i <<- inverse
	    getinverse <- function() i
	    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" object created with makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data...")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}

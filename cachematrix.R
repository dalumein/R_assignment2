## Week 3 assignment: Caching the Inverse of a Matrix

## Generate a function called makeCacheMatrix: this function creates a special “matrix” object, then set and get the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	
	##from the example, replace m with "inverse", it starts as NULL
	inverse <- NULL
	
	#set x in parent env with y and reset the inverse
	set <- function(y = matrix()) {
	x <<- y
	inverse <<- NULL
	}

	##get
	get <- function() x

	#set and get inverse 
	setinverse <- function(new_inverse) inverse <<- new_inverse
	getinverse <- function() inverse
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Generate a function called cacheSolve: this function computes the inverse of the matrix returned by the above makeCacheMatrix. If the cached inverse already exists, the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        
	##check the existence 
	checkinverse <- x$getinverse()
	##found the matrix
	if(!is.null(checkinverse)) {
		    message("We found cached matrix!")
		    return(checkinverse)
	}
	##otherwise get the matrix
	matrix2solve <- x$get()
	checkinverse <- solve(matrix2solve, ...)
	x$setinverse(checkinverse)
	checkinverse
}


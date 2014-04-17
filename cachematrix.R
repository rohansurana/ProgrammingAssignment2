## Matrix inverse is usually a costly computation.
## These functions cache the inverse of matrix rather than computing it repeatedly

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## It creates a matrix object which can perform the following functions-
## 1. Set the value of matrix to another value
## 2. Get the value of matrix 
## 3. Set the inverse of the matrix to the value supplied
## 4. Get the inverse of the matrix stored in the object

makeCacheMatrix <- function(x = matrix()) {

	## 1. Set the value of matrix to another value 
	  inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

	## 2. Get the value of matrix 
        get <- function() x

	## 3. Set the inverse of the matrix to the value supplied
        setinverse <- function(inv) inverse <<- inv

	## 4. Get the inverse of the matrix stored in the object
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  inverse <- x$getinverse()

	  ## If inverse has already been computed (cached) then simply return it	
        if(!is.null(inverse)) {
		    message("Getting cached data")
                return(inverse)
        }
        data <- x$get()

	  ## Computing inverse of matrix
        inverse <- solve(data, ...)

	  ## Set inverse of matrix
        x$setinverse(inverse)

	  ## Return the inverse
        inverse
}
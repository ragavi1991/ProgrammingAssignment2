## Matrix inversion

## This function creates a special "matrix" object that can cache its inverse
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
	## set the value of the matrix
	## get the value of the matrix
	## set the value of the inverse matrix
	## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {           # Input will be a matrix
        m <- NULL                                     # m stores the inverse matrix. It is set to null everytime makeCacheMatrix function is called.
	set <- function(y){                           # This function takes the value of original matrix
		x <<- y                               # Stores the value of orginal matrix to x
		m <<- NULL                            # Sets value of m to NULL
	}
	get <- function() x                           # This function returns the value of the original matrix
	setsolve <- function(inv) m <<- inv           # This function will access and store the value of inverse matrix
	getsolve <- function() m                      # This function returns the stored value of inverse matrix
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) # List of internal functions for calling functions to access the functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix value from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {                     # Input will be the object created by makeCacheMatrix function
        m <- x$getsolve()                            # Accesses getsolve function and gets inverse matrix
	if(!is.null(m)){                             # Checks if the inverse matrix is computed already or not
		message("getting cached matrix")     # If inverse matrix already exists, it displays the message
		return(m)                            # and returns the inverse matrix
	}
	data <- x$get()                              # If inverse doesn't exist, it gets the value of original matrix 
	m <- solve(data)                             # Computes the inverse of the matrix, if inverse doesn't exist
	x$setsolve(m)                                # then, store the computed inverse matrix to m
	m                                            # Returns m, which has the inverse matrix
}

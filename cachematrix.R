## File Content: 	makeCacheMatrix and cacheSolve
## Application:	To generate a special matrix object that maintains a
##			copy of a matrix and its inverse (after being calculated once) 
##


## makeCacheMatrix: A function for creating a "CacheMatrix" object that holds a
##			matrix and its inverse (when it is set once), and functions for 
##			accessing and setting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix(0)) {
		## Initializing xinv (x is initialized through function's argument)
		xinv <- NULL
		## Matrix Element Manipulation: Set the value of x
		set <- function(mat) {
				x <<- mat
				xinv <<- NULL
		}
		## Matrix Element Manipulation: Get the value of x
		get <- function() x
		## Matrix Inverse Manipulation: Set the value of xinv
		setinv <- function(matinv) xinv <<- matinv
		## Matrix Inverse Manipulation: Get the value of xinv
		getinv <- function() xinv
		## Making manipulation/access functions available
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: A function for returning the inverse of a matrix held in a "CacheMatrix" 
##		object x. The inverse is retrieved from the cache if available, or is first
##		calculated and then cached for future references.
cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of the matrix in CacheMatrix object 'x'
		## First, try to retrive the inverse
		inv <- x$getinv()
		if (!is.null(inv)) {
			message("Getting cached data")
			return(inv)
		}
		## If not available, try to calculate the inverse
		data <- x$get()
		## Calculate and cache the inverse
		x$setinv(solve(data))
		## return the calculated inverse
		x$getinv()
}

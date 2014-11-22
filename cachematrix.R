##Caching the inverse of a matrix rather than compute it repeatedly 

## Special function to store matrix and matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {	##input for matrix data
		x <<- y
		inv <<- NULL ## Reset inverse because we have created new matrix.
	}
	get <- function() x	##output for matrix data
	setinverse <- function(inverse) inv <<- inverse ## input for inverse
	getinverse <- function() inv	## output for inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Calculates inverse of matrix and stores it in special matrix object and then returns it.
## If matrix inverse is already available from object it just returns result.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")	##object already had calculated information
		return(inv)	##just return it
	}	
	data <- x$get()	## get data to calculate inverse
	inv <- solve(data, ...)	## actual calculation
	x$setinverse(inv)	## setting result to special matrix object
	inv	## returning result
}
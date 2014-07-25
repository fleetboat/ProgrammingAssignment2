## Functions to cache the inverse of a matrix
## 

## Create a list with functions we need to cache inverse

makeCacheMatrix <- function(x = numeric()) {   # We assume input is already a matrix
	m <- NULL                                  # Assign NULL value to m
	set <- function(y) {                       # Define set function which
		x <<- y                                # caches values of x and initial
		m <<- NULL                             # NULL value for m
	}
	get <- function() x                        # Define get function to return x
	setinv <- function(inverse) m <<- inverse  # Function to put inverse value into m
	getinv <- function() m                     # Function to return value of m
	list(set = set, get = get,                 # Create a list of these functions
	     setinv = setinv,
	     getinv = getinv)
}


## If no value of the inverse is cached, compute it

cacheSolve <- function(x, ...) {
    m <- x$getinv()                           # m will either have NULL or the inverse
	if(!is.null(m)) {                         # Alert the user if using a cached value
		message("getting cached data")
		return(m)
	}
	data <- x$get()                           # Get the input matrix
	m <- solve(data, ...)                     # Find the inverse
	x$setinv(m)                               # Put inverse value into m
	m                                         # Print the inverse
}

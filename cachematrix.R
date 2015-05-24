## function makeCacheMatrix creates a special "matrix" object that function cacheSolve
## is able to cache the "matrix" inverse in the cache when the inverse has been
## calculated.

## A function to create a special "matrix" object that can cache it inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setsolve <- function(solve) m<<-solve
	getsolve <- function() m
	list(	set = set, get = get,
	     	setsolve = setsolve,
		getsolve = getsolve)
}


## A function to compute the inverse of the special "matrix" returned by makeCacheMetrix
## above. If the inverse has already been calculated, then the cachedsolve should
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
	m <-x$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
	m
}

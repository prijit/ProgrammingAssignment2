## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will output a special matrix that has the ability to store the environment using closure.
## get - returns the original matrix
## set - set the matrix and sets inverse as NULL
## setinverse - cache the inverse
## getinverse - returns the cached inverse or NULL
makeCacheMatrix <- function(x = matrix()) {
 	inverse <- NULL
    set <- function(y) {
    	## Change value of x to be different from the time closure was created
        x <<- y
        inverse<<- NULL
    }
    get <- function() {
    	## returns the original matrix
    	x
    }
    setinverse <- function(i) {
    	## cache the inverse
    	inverse <<- i
    }
    getinverse <- function() {
    	## returns the cached inverse or NULL
    	inverse
    }
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Check to see if the inverse is already cached.
## If inverse is cached then return the cache
## If it is not cached then calculate inverse using solve function, cache the inverse then return the inverse
cacheSolve <- function(x, ...) {

    ## If inverse is cached then return the cache
	inverse <- x$getinverse()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }

    ## Inverse is not cached so calculate inverse
    data <- x$get()
    inverse <- solve(data, ...)

    ## Cache inverse
    x$setinverse(inverse)
    inverse
}

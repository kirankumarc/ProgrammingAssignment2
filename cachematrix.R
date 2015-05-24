
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
	##	It creates a special "vector", which is really a list containing a function to
		##	1. set the value of the vector
		##	2. get the value of the vector
		##	3. set the value of the mean
		##	4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data.")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver
}

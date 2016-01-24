## Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.
## The first function, makeVector creates a special "vector" and the second function "cacheSolve" calculates the inverse of the of matrix. if this has already been calculated it prints from cache


## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse
## step 1: set the value of the vector
## step 2: get the value of the vector
## step 3: set the value of the mean
## step 4: get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	
	setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse
			 )
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
	## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("this is cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

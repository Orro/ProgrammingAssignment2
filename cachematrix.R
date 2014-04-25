## The following two functions will create an object that stores a matrix 
## and cache its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to
## set the value of the matrix,
## get the value of the matrix,
## set the value of the inverse, and
## get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## variable that stores the inverse
    invMat <- NULL
    ## function that stores the matrix in the variable set
    set <- function(y) {
     
     	x <<- y
    
    	invMat <<- NULL
    }
    ## function that returns the matrix
    get <- function() x
    ## function that caches the inverse in the variable invMat
    setInvMat <- function(inverse) invMat <<- inverse
    ## function that returns the inverse
    getInvMat <- function() invMat
    
    list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}



## The second function, cacheSolve calculates the inverse of the matrix created 
## with the previous function. This function first checks if the inverse has already 
## been calculated. If so, it just gets it from the cache. If not, the inverse is 
## calculated and its value set to the cache. 

cacheSolve <- function(x, ...) {
    ## store the inverse in the variable invMat   
    invMat <- x$getInvMat()
    ## loop that checks if the inverse is in the cache
    if (!is.null(invMat)) {
   
        message("getting cached data")
   
        return(invMat)
    }
   
    else {
    ## get the matrix from makeCacheMatrix   
    data <- x$get()
    ## calculate the inverse
    invMat <- solve(data, ...)
    ## set the inverse in the cache
    x$setInvMat(invMat)
    ## return the inverse
    invMat
	
	}
}

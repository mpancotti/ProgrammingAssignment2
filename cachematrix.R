## The functions do more or less the same actions that the example does, only using the
## solve function instead of the mean function

## this function return a list of methods (get, setinverse and getinverse) that will be used 
## by the cacheSolve function. 
##
## The getInverse function return the cachedInvertedMatrix if it has been already calculated. 

makeCacheMatrix <- function(x = matrix()) {
    cachedInvertedMatrix <- NULL
    get <- function() {x}
    setinverse <- function(invertedMatrix) cachedInvertedMatrix <<- invertedMatrix
    getinverse <- function() cachedInvertedMatrix
    list(get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the methods created in the makeCacheMatrix function in order to use or set the 
## cachedInvertedMatrix instead of calculate all the time. In ths way the solve function will be applied only the
## first time you calculate the inverted value of a specific matrix. The subsequent times you need the same inverted
## matrix the cached value will be used

cacheSolve <- function(x, ...) {
    invertedMatrix <- x$getinverse()
    if(!is.null(invertedMatrix)) {
      message("getting cached data")
      return(invertedMatrix)
    }
    data <- x$get()
    invertedMatrix <- solve(data, ...)
    x$setinverse(invertedMatrix)
    invertedMatrix
}

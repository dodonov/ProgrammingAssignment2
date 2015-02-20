## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- x
    cachedResult <- NULL
    set <- function(newMatrix) {
        cachedMatrix <<- newMatrix
        cachedResult <<- NULL
    }
    get <- function() cachedMatrix
    setsolve <- function(solve) cachedResult <<- solve
    getsolve <- function() cachedResult
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    result <- x$getsolve()
    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    matrix <- x$get()
    result <- try(solve(matrix, ...))
    x$setsolve(result)
    
    result
}

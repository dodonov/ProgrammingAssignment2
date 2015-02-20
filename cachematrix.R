## This solution is pretty close to the sample with cached mean of a vector.
## I`ll try to explain each line of code for better understanding of this
## solution.

## makeCacheMatrix returns a list of auxiliary funtions which is used to access
## or save the values of original matrix and its inversion. Also this function
## creates environment where above values will be stored. So, returned value
## will behave as a container for our values with appropriate functions for 
## access to them.
## The only argument of the function is used to pass the value of the original
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Save the value of the original matrix in the current environment.
    ## We could just use the variable `x` for this purpose, but I
    ## prefer to make a separate variable with clear name to make
    ## code more readable.
    cachedMatrix <- x
    
    ## Create and initialize the variable for the cached result.
    cachedResult <- NULL
    
    ## Creates the `set` function which can be used to change saved value
    ## of the original matrix. Also it will invalidate any cached result
    ## if it was calculated before.
    set <- function(newMatrix) {
        ## Update our saved original matrix with the new value.
        ## We use `<<-` operator because we want to force R to find variable
        ## in parent environments, instead of just creating a new variable
        ## in current environment, as it would do if we use `<-` operator.
        ## So R will find `cachedResult` variable defined above in the
        ## environment of the function `makeCacheMatrix` and assign to it the
        ## value of the `newMatrix` variable.
        cachedMatrix <<- newMatrix
        ## Reset cached result.
        cachedResult <<- NULL
    }
    
    ## `get` just returns the value of our cached original matrix.
    ## I'd like to note that in this case cachedMatrix is a free variable,
    ## because it wasn`t defined in `get` function. So R will look for it in the
    ## parent environments and eventually will find it in the environment of the
    ## `makeCacheMatrix` function.
    get <- function() cachedMatrix
    
    ## `setsolve` is used to save (or cache) result of matrix inversion.
    ## Result will be saved in the variable `cachedResult` of `makeCacheMatrix`
    ## environment.
    setsolve <- function(solve) cachedResult <<- solve
    
    ## `getsolve` is used to get the result of matrix inversion previosly saved
    ## with `setsolve` function.
    getsolve <- function() cachedResult
    
    ## Return list of created functions.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## `cacheSolve` returns inversion of matrix and cache it. If it was solved
## earlier then it will return the cached value.
## As argument it can accept only "containers" creates with `makeCacheMatrix`
## function.

cacheSolve <- function(x, ...) {
    
    ## Get the cached result
    result <- x$getsolve()
    ## if there is something
    if(!is.null(result)) {
        ## then return it and exit
        message("getting cached data")
        return(result)
    }
    ## Otherwise, get the original matrix
    matrix <- x$get()
    ## Calculate inversion
    result <- try(solve(matrix, ...))
    ## and save the result to the cache
    x$setsolve(result)
    
    ## Return the result
    result
}

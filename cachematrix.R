## Put comments here that give an overall description of what your
## functions do
## Below are two functions that are used to create a special object that stores 
## an invertible matrix  and cache's its inverse

## Write a short comment describing this function
##The first function below, makeCacheMatrix creates a special "matrix", 
#which is really a list containing a function to
#1 set the value of the invertible matrix
#2 get the value of the matrix
#3 set the value of the Inverse
#4 get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
##the <<- operator which is used to assign a value to an object in an environment 
## that is different from the current environment. So, both X and inv  below are in
## a different environment than this
                                x <<- y
                                inv <<- NULL
                }
        get <- function() x
        setInv <- function(invrs) inv <<- invrs
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv,  getInv = getInv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, 
##it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value 
##of the inverse in the cache via the setInv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        return(inv)
}


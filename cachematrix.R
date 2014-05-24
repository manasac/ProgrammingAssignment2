## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes in a matrix and gives out a list of four functions.
## set function sets the variables of parent environment 
## get function returns the matrix passed into makeCacheMatrix
## setinv sets the variable m in parent env
## getinv gets the variable m in parent env
## makeCacheMatrix uses the variable in parent environment to cache
## the inverse of matrix.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    #variable m in parent environment is assigned to the calculated inverse of matrix
    setinv <- function(inv) m <<- inv
    
    #variable m in parent environment is returned by getinv
    getinv <- function() m
    
    #makeinv returns list of four functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of a matrix and stores the value, if there
## is no cached value. It returns the cached value when there is cached value 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    #check if the inverse is available in m of parent env.
    if(!is.null(m)) {
        #if m of parent env is cached with the matrix inverse 
        message("getting cached data")
        #return the cached value of m
        return(m)
    }
    data <- x$get()
    
    #ginv is used to inverse matrix of n rows and mcolumns
    #solve function is restricted to inversing a square matrix
    #library(MASS) needs to be run for ginv function
    m <- ginv(data)
    
    x$setinv(m)
    m
}

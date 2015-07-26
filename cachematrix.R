## This program creates a matrix and permits caching of the inverse.   ## You can set or retrive the values of the matrix and inverse.   

## Function take a matrix and creates a set of functions to
##   get - sets matrix value 
##   set - get value of input matrix
##   setInverse - Value is set to input value, not the inverse
##   getInverse - Get inverse value of input matrix 
##   usage:   output < - makeCacheMatrix(testdata) where output is a
##   matrix and testdata is a existing matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x 
        setInverse <- function(foo)   m <<- foo 

        getInverse <- function() m

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function returns inverse of input matrix from cache if it already ## exists.  Otherwise, it calculates inverse of input matrix, updates ## cache, and returns inverse 
## usage:   cacheSolve(test1) where test1 is a matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
             if(!is.null(m)) {
             print("getting Inverse of matrix")
             return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}
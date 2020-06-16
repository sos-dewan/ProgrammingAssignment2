## These function makes the use of lexical scoping in R, which helps to cache the 
## required value and use it later instead of calculating the value again

## This first function takes a matrix as an argument and calculates it's inverse and 
## caches the value. The function is similar to example in vector, and I have made
## the necessary changes in variable names, data type and functions called.

makeCacheMatrix <- function(x = matrix()) 
{
     inv <- NULL
     set <- function(y)
     {
         x <<- y
         inv <<- NULL
     }
     get <- function()x
     setInverse <- function(solve) inv <<- solve
     getInverse <- function() inv
     list(set =set, 
          get = get,
          setInverse=setInverse,
          getInverse=getInverse)
}


## This functions takes in an argument x (matrix in our case) and tries to get
## its inverse if there exists one. But if the inverse is null then this function
## calculates the inverse and returns the inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat,...)
    x$setInverse(m)
    m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("gettgin cached data")
        return(m)
    }
    mat <- x$get()
    m <- solve(mat,...)
    x$setInverse(m)
    m
}

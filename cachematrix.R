## Functions to compute the inverse of a matrix and cache the result.

## Create a special "matrix" object, which is really a list containing
## functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix 
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        ## Set value of matrix, set m to NULL since matrix has changed
        x <<- y
        m <<- NULL
    }
    ## Return current matrix
    get <- function() x
    ## Set inverse matrix
    setinverse <- function(inverse) m <<- inverse
    ## Return current inverse matrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    
    ## Check if inverse has already been computed
    m <- x$getinverse()
    if(!is.null(m))
    {
        ## If it has return inverse matrix
        message("getting cached data")
        return(m)
    }
    ## If not compute and set the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m        
}

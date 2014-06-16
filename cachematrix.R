## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. This R module contains a pair of functions that cache 
## the inverse of a matrix.

## The first function, makeVector creates a "special" matrix, which is really a list 
##   containing a function to:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the matrix inverse
## 4 - get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    ## clear the inverse
    m <- NULL
    
    ## provide the setter function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## provide the getter function
    get <- function() x
    
    ## provdide the set inverse function
    setinverse <- function(inverse) m <<- inverse
    
    ## provide the get inverse function
    getinverse <- function() m
    
    # define the API
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second funciton, cacheSolve provides the inverse of the suppliedmatrix. 
## If the inverse has already been calculated, it returns the cached inverse,
## else it calculates (and caches) it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## retrieve the inverse
    m <- x$getinverse()
    
    ## was the inverse stored?
    if(!is.null(m)) {
        ## yes - just return it
        return(m)
    }
    else
    {
        ## no, we'd better create it then
        
        ## retrieve the matrix
        data <- x$get()
        
        ## generate the inverse
        m <- solve(data, ...)

        ## cache the inverse
        x$setinverse(m)
        
        ## return the inverse
        return(m)
    }
    
    
}

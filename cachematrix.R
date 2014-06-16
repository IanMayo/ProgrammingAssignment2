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
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## The second funciton, cacheSolve provides the inverse of the suppliedmatrix. 
## If the inverse has already been calculated, it returns the cached inverse,
## else it calculates (and caches) it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
    
}

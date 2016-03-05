## Matrix inversion is usually a costly computation.  Here we take advantage of caching the inverse of a matrix
##   rather than compute it repeatedly.  This is accomplished by the following two functions makeCacheMatrix()
##   and cacheSolve().

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix().
##   If the inverse has already been calculated (and the matrix has not changed), then cachesolve() 
##   retrieves the inverse from the cache.  Return a matrix that is the inverse of 'x'To compute the 
##   inverse of a square matrix, we will be using the solve() function in R.
##   Known limitation: This function only works when the matrix supplied is invertible.
cacheSolve <- function(x, ...) {
    m <-x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

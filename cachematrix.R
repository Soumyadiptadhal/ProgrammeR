## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. These pair of functions cache the inverse of a matrix, and
## display the cached data, if available, when called for.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL
        set<- function(y)
        {
                x <<- y
                inverse <- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set=set, get= get,
             setInverse =setInverse, getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) 
{
        inverse <- x$getInverse()
        if(!is.null(inverse))
        {
                message("getting cached data")
                return(inverse)
        }
        input_matrix <- x$get()
        inverse <- solve(input_matrix,...)
        x$setInverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

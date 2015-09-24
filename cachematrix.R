## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
#inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion 
#that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverseResult <- NULL # set the inverseResult to NULL as a placeholder for future use
        set <- function (y){ 
                x <<- y
                inverseResult <<- NULL
        } # this set the verctor X to a new vector Y and inverseResult to NULL
        
        get <- function () x    #Returns the vector x
        setInverse <- function(inverse)  inverseResult <<- inverse     #Set the inverse of a matrix to inverseResult
        getInverse <- function() inverseResult        #Get the inverse of a matrix, inverseResult
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)  #returns a 'special vector' containing all the functions just defined
        
} 


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse() 
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        } 
        dataMat <- x$get()
        inv <- solve(dataMat, ...) # 'solve' gives the inverse of the given matrix
        x$setInverse(inv)
        inv
}

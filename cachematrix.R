makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse as NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset inverse when matrix is updated
        }
        
        get <- function() x  # Return the matrix
        
        setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
        
        getInverse <- function() inv  # Get the cached inverse
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if (!is.null(inv)) {
                message("Getting cached inverse")
                return(inv)  # Return cached inverse if available
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)  # Compute the inverse
        x$setInverse(inv)  # Store the inverse in cache
        inv  # Return the computed inverse
}
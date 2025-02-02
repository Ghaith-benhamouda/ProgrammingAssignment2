#' Caching the Inverse of a Matrix
#' 
#' Matrix inversion is computationally expensive, so caching the inverse 
#' can improve efficiency when the same matrix inversion is required multiple times. 
#' The following functions create a special "matrix" object that caches its inverse.

#' Create a special matrix object that can cache its inverse
#'
#' This function creates a list containing functions to:
#' 1. Set the matrix
#' 2. Get the matrix
#' 3. Set the cached inverse
#' 4. Get the cached inverse
#'
#' @param x A square, invertible matrix (default is an empty matrix).
#' @return A list of functions to interact with the matrix and its cached inverse.
#' @examples
#' m <- matrix(c(2, 2, 1, 4), nrow = 2, ncol = 2)
#' cm <- makeCacheMatrix(m)
#' cm$get()  # Retrieve the matrix
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

#' Compute and retrieve the cached inverse of a matrix
#'
#' This function computes the inverse of a matrix returned by `makeCacheMatrix()`. 
#' If the inverse has already been computed and stored in the cache, it retrieves 
#' the cached value instead of recomputing it.
#'
#' @param x The special "matrix" object created by `makeCacheMatrix()`.
#' @param ... Additional arguments passed to the `solve()` function.
#' @return The inverse of the original matrix.
#' @examples
#' m <- matrix(c(2, 2, 1, 4), nrow = 2, ncol = 2)
#' cm <- makeCacheMatrix(m)
#' cacheSolve(cm)  # Compute and store the inverse
#' cacheSolve(cm)  # Retrieve cached inverse
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

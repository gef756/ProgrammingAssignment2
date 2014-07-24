# R Programming, Assignment 2

# ================================================================
#
# Functions for creating and operating on a matrix to calculate
# an inverse, using a cached value if available to reduce
# computation time.
#
# ================================================================

# Function for a special matrix object that can cache its inverse 
#
# param x: initial matrix

makeCacheMatrix <- function(x = matrix()) {
    # Initialize inverse as uncalculated
    inv <- NULL
    
    # Set new value for matrix
    Set <- function(new.matrix) {
        x <<- new.matrix
        inv <<- NULL
    }
    
    # Getter for matrix
    Get <- function() x
    
    # Set the inverse matrix
    SetInv <- function(new.inv) {
        inv <<- new.inv
    }
    
    # Get the inverse matrix
    GetInv <- function() inv
    
    # Return the rest
    list(Set = Set, Get = Get,
         SetInv = SetInv,
         GetInv = GetInv)
}

# Return the inverse of the matrix returned by makeCacheMatrix
# 
# If the inverse has already been calculated and the matrix
# hasn't been changed, the function will return a cached result.
#
# Otherwise, the function will recalculate the inverse.
#
# Assumption: the matrix is invertible
#
# param x : a matrix that comes from makeCacheMatrix
# param ... : arguments to function x

cacheSolve <- function(x, ...) {
    # Get inverse value stored
    inv <- x$GetInv()
    
    # If inverse is not null, cache is valid. So, return that.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If it's null, cache is invalid. So, (re)calculate.
    # Then, store it in the cache, and return it.
    data <- x$Get()
    inv <- solve(data, ...)
    x$SetInv(inv)
    inv
}

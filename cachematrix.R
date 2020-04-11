## Put comments here that give an overall description of what your
## functions do

## This functions were written for the Week 3 assignment
## of the R Programming course (Coursera)
## Written by github user ShotsAndScience; 11.04.2020

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {           ## defines the argument with a default mode of "matrix
        z <- NULL                                     ## initialize z as null which also holds the inverse matrix
        set <- function(y) {                          ## defines the set function to assign a new value of matrix 
            x <<- y
            z <<- NULL                                ## reset z to NULL if there is a new matrix
        }
        get <- function() x                           ## defines the get function to return the value of the matrix
        setinverse <- function(inverse) z <<- inverse ## assigns the value of z in the parent environment
        getinverse <- function() z                    ## gets the value of z
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }


## Write a short comment describing this function
## This function computes the inverse of the matrix returned by the above function
## If the matrix has not changed and the inverse has already been calculated, cacheSolve will
## retrieve this inverse from the cache

cacheSolve <- function(x, ...) { ## returns a matrix which is the inverse of "x"
    z <- x$getinverse()
    if (!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinverse(z)
    z
        
}

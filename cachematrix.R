## This file content is functions that allows to create a special matrix that
## can cache its inverse and a function that calculate the inverse is not yet
## already calculated


## creates a special matrix object that can cache its inverse
##input: a matrix
##output: a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special matrix created with makeCacheMAtrix
## if the inverse matrix already exists, retrive inverse from cache
##input: special matrix
##output inverte of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


## Function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse, and function cacheSolve returns de inverse of a 
## special "matrix" created whith makeCacheMatrix.

## Function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse. It contains a matrix, 
## its inverse and a list of functions to get and set these matrices: 
## set, get, setinverse, getinverse
##
## 'x' is an invertible matrix. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}

## Returns a matrix that is the inverse of the matrix in 'x'.
## If the inverse has already been calculated,
## then retrieves the inverse from the cache, and shows a message.
## 
## 'x' is an object created with makeCacheMatrix containig a matrix 
## (see function makeCacheMatrix), assuming that the matrix supplied is 
## always invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
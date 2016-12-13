## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## x is a square invertible matrix 
## makeCacheMatrix is a function that returns a list to: 
## 1. set the matrix
## 2. get the matrix 
## 3. set the inverse
## 4. get the inverse

## define makeCacheMatrix as a function about a matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL 
        set <- function(y) {
## assign a value to an object in an environment different from the current environment
                x <<- y 
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
## return a list to 
## 1. set the matrix
## 2. get the matrix 
## 3. set the inverse
## 4. get the inverse
             list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## x is the output of makeCacheMatrix()
cacheSolve <- function(x, ...) {
       i <- x$getinverse() ## return the inverse of the original matrix input to makeCacheMatrix
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

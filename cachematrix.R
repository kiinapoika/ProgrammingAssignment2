## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## x is a square invertible matrix 
## makeCacheMatrix is a function that returns a list to: 
## 1. set the matrix
## 2. get the matrix 
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## define makeCacheMatrix as a function about a matrix
        i <- NULL 
        set <- function(y) {
                x <<- y 
                i <<- NULL
                ## assign a value to an object in an environment different from the current environment
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function that computes the output of makeCacheMatrix
cacheSolve <- function(x, ...) {
       i <- x$getinverse() 
        if(!is.null(i)) { ## if the inverse of the original matrix has already been stored in cache
                message("getting cached data") ## return the message "getting cached data"
                return(i)  ## and the inverse   
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that 
makeCacheMatrix <- function(x = matrix()) { ## define makeCacheMatrix as a function about a matrix
        i <- NULL ## assign the value of the variable i to be NULL
        set <- function(y) {
                x <<- y ## update the value the original matrix
                i <<- NULL ## update the value of the variable i to be NULL in the current environment
        }
        get <- function() x ## return the original matrix
        setinverse <- function(inverse) i <<- inverse ## motify the value of variable i to inverse
        getinverse <- function() i ## return the value of i 
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
## returns a list to: 
## 1. set the matrix
## 2. get the matrix 
## 3. set the inverse
## 4. get the inverse
}

## Write a short comment describing this function
## cacheSolve is a function that computes, caches and returns the inverse of the matrix that is created in makeCacheMatrix()
cacheSolve <- function(x, ...) {
       i <- x$getinverse() ## assign the variable i to the function that returns the inverse of the original matrix
        if(!is.null(i)) { ## if the inverse has already been stored in cache
                message("getting cached data") ## return the message "getting cached data"
                return(i)  ## and the inverse 
        }
        data <- x$get() ## return the original matrix
        i <- solve(data, ...) ## return the matrix inverse
        x$setinverse(i) ## modify existing matrix inverse
        i ## print new inverse
}

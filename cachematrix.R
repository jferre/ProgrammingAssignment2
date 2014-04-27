## makeCacheMatrix and cacheSolve are a pair of functions that...
## ...cache the inverse of a matrix.

## makeCacheMatrix creates a special vector object with four functions:
## 1.  set the value of the Matrix
## 2.  get the value of the Matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                        #set function 
        x <<- y
        m <<- NULL
    }
    get <- function() x                         #get function
    setinverse <- function(solve) m <<- solve   #setinverse function
    getinverse <- function() m                  #getinverse function
    list(set = set, get = get,                  #output list, this is used by cache solve
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve uses the output from makeCacheMatrix. 
## First checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value...
## of the inverse matrix in the cache via...
## the setinverse function.m <- x$getinverse()
cacheSolve <- function(x, ...) {
    m <- x$getinverse()                 #Query the x vector's cache, from the output of makeCacheMatrix
    if(!is.null(m)) {                   #If there is a cache...
        message("getting cached data")  #...print "getting cached data" and...
        return(m)                       #... return the cache. Note that no computation is performed.
    }
    data <- x$get()                     #If there's no cache, from the output of makeCacheMatrix...
    m <- solve(data, ...)               #...then compute the inverse of the matrix...
    x$setinverse(m)                     #...,save the result back to x's cache...
    m                                   #... and lastly return the inverse.
}

## Code for testing purposes, rerun of creating test3 should print "getting cached data"
#test <- matrix(data = c(1,0,5,2,1,6,3,5,0),nrow = 3, ncol = 3) 
#test2 <- makeCacheMatrix(test)
#test3 <- cacheSolve(test2)
#test3 <- cacheSolve(test2)
## second test3 prints "getting cached data".







## These 2 functions serve as a way to calculate and store/cache the inverse of  
## a matrix.
## Doing it in this way can reduce the computer intensity during matrix 
## inversion. :)

## "makeCacheMatrix" is a function that stores the cached data which in this
## case, is the inverse of the matrix entered. 

makeCacheMatrix <- function(x = matrix()) {
# This function creates a special "matrix" object that can cache its inverse.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## "cacheSolve" is a function that calculates the inverse of the matrix in the 
## event that it is not already found in the cache ("makeCacheMatrix"). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message ("getting cached data")
        return (m)  # if cached data is available, get it. else calculate again
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Example 
## sample <- matrix(c(2,0,0,0,1,0,0,0,1),3,3)
## calinv <- makeCacheMatrix(sample)
## cacheSolve(calinv)
## calinv$getinverse()

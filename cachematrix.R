## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the matrix matrix of the matrix
## 4.get the matrix matrix of the matrix



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The following cacheSolve function calculates the 
## matrix matrix of the special "matrix"
## created with the above makeCacheMatrix function.
## It will first check to see if the matrix has already been calculated. 
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the matrix of the data and sets 
## the value of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m

}

## Return a matrix that is the inverse of 'x'
## > solve(x)
##      [,1]
## [1,]  0.2
## > cacheSolve(makeCacheMatrix(x))
##      [,1]
## [1,]  0.2
## The given assignment involves creating two functions designed to efficiently 
## handle the inversion of a matrix by caching the result to avoid redundant 
## calculations.

## "makeCacheMatrix" generates a special "matrix" object capable of caching its 
## inverse. It sets up a structure to store a matrix and its inverse. 
## The structure includes methods to set and get the matrix, as well as to set 
## and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## "cacheSolve" computes the inverse of the special "matrix" object created by 
## "makeCacheMatrix." It first checks if the inverse has already been calculated 
## and stored in the cache. If so, it retrieves the cached inverse to save 
## computation time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

# Testing the Functions
a <- matrix(rnorm(25),5,5)
a

b <- makeCacheMatrix(a)
b
cacheSolve(b)

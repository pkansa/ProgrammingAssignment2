## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly.  This  pair of functions will cache
## the inverse of a matrix

## makeCacheMatrix will do the following:
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse of the matrix
## 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inverted <- NULL
                set <- function(y) {
                        x <<- y
                        inverted <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inverted <<- inverse
                getinverse <- function() inverted
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## This next function will generate the inverse of a matrix.  It first checks to see if the inverse
## haS already been generated (and cached).  If it has, it will return it; otherwise, it will run
## the inverse through, cache it, and return the results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getinverse()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <-solve(data, ...)  ## solve is what does the inverting
        x$setinverse(inverted)
        inverted
}

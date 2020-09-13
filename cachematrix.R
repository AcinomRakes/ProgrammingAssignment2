## The following two functions together can be used to compute the inverse of a 
## matrix and further caching this inverse for further use to avoid computing
## the same inverse matrix repeatedly

## The makeCacheMatrix function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {         ## x gets initialized as a formal argument
    inv <- NULL                                     ## inv to contain the inverse matrix initialized as NULL
    set <- function(y) {                            ## to allow for resetting the value with a new matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                             ## function to get matrix x 
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv                        ## function to get the inverse of the matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes and returns the inverse of the matrix 'x' 
## returned by makeCacheMatrix above if it has not been calculated previously,
## and fetches the value from cache if has been calculated previously.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){                              ## checking if inverse matrix has been computed before
      message("getting cached data")
      return(inv)                                   ## returning previously computed matrix, if any
    }
    data <- x$get()
    inv <- solve(data, ...)                         ## computing the inverse matrix
    x$setinv(inv)
    inv                                             ## returning the inverse matrix
}

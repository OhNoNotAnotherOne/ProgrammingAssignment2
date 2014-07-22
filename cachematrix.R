## These functions perform matrix inversion on an n x n matrix, where the matrix
## is assumed to be invertible. Matrix inverses are cached such that calling the
## cacheSolve function does not need to recompute the matrix inverse if the
## original matrix is unchanged.

## This function creates a list from the provided matrix x. The list provides
## access to functions that set and get the matrix, and set and get the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix())
{
    inv <- NULL
    setmatrix <- function(y)
    {
        x <<- y 
        inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a list object created by makeCacheMatrix and uses it to
## compute the inverse of the matrix stored in the list object. If the inverse
## has already been computed, and the matrix has not changed, then a cached
## version of the inverse is returned rather than recomputing the inverse. The
## further arguments indicated by the ellipsis are passed on to the solve
## function.

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("retrieving cached matrix")
        return(inv)
    }
    mat <- x$getmatrix()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}

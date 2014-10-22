
## You create a special "matrix" object by calling makeCacheMatrix on
## an invertible matrix e.g.
## my_matrix <- matrix(c(2, 0, 1, 4), nrow = 2, ncol = 2)
## special_matrix_object <- makeCacheMatrix(my_matrix)
## (special_matrix_object is a List of 4)
## Next call cacheSolve function on your special_matrix_object.
## The first time you call 
## cacheSolve(special_matrix_object)
## the inverse of my_matrix is calculated and returned. As long as your
## special_matrix_object is unchanged, subsequent calls of
## cacheSolve(special_matrix_object) will print a string
## "getting cached data" and return the already solved inverse
## of my_matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve
##the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

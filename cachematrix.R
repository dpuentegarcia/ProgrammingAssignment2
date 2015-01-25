# Matrix inversion is often a time consuming computation and it is
# useful to cache the inverse of a matrix computed previously rather than
# compute it again. These  two functions allow to cache the inverse of a matrix.
·
# makeCacheMatrix creates a list with a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse_matrix) inv <<- inverse_matrix
    getinverse <- function() inv
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# The following function calculated the inverse of the matrix. It 
# checks if the inverse has already been computed. If so, it gets 
# the result. If not, it computes the inverse, sets the value in the 
# cache through the setinverse function.
# The input matrix is assumed to be  invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
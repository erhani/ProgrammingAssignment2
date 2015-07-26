## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {

        ## define the cache
        cache <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the
                cache <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) cache <<- inverse
        ## to the inverse of the matrix x
        getinverse <- function() cache
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cache <- x$getinverse()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }

        data <- x$get()
        cache <- solve(data, ...)
        x$setinverse(cache)
        cache
}
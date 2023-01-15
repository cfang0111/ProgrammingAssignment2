## CFang assignment 2 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {  ##set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x    ##get the value of the matrix
        setinverse <- function(inverse) m <<- inverse  ##set value of inverse
        getinverse <- function() m ##get value of inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Function computes the inverse matrix, then retrieve the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

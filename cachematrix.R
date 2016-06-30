## This funciton is to calculate the inverse of a matrix, and store it in memeory
## Once the inverse of the matrix is stored, so next time user can just get the inverse from the cache instead of calculating it

## Creating matrix and cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## First chech whether the inverse of the matrix is calculated
## If it is already calculated, get the vaule
## If not, calculate the inverse and then store the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

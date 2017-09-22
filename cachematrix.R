## Put comments here that give an overall description of what your
## functions do

## similar to 'mean'example: 
## 1. set the data of the matrix
## 2. get the data of the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                   ## 1. set the data of the matrix
            x <<- y
            m <<- NULL
        }
        get <- function() x                    ## 2. get the data of the matrix
        setinverse <- function(inv) m <<- inv  ## 3. set the inverse
        getinverse <- function() m             ## 4. get the inverse
        list(set = set, get = get,             ## output list
             setinverse = setinverse,
             getinverse = getinverse)
}


## instead of the mean funtion, now the solve function is used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,diag(nrow(data)), ...)         ## solve calculates inverse. 
        x$setinverse(m)
    m
}

## Two functions were written to create a special object and cache the inverse of a matrix.
## If the inverse of a matrix was already calculated,caching it rather than compute it repeatedly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## Generateing a list of four functions(set,get,setinverse, and getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(minverse) m <<- minverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
 }


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated,using getinverse to get it from cache. 
## If the inverse is not calculated, using solve function to calculate it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
 }

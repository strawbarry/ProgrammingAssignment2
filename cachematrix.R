## The following 2 functions cache the inverse of a matric.

## This function, "makeCacheMatrix", creates a special "matrix" that
## is actually a list that contains functions to
## 1. set the value of the matrix
## 2. get the value of the matric
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function, "cacheSolve", calculates the inverse of the special "matrix
## created by the above function. It first checks if the inverse has already
## been calculated. If so, it gets the inverse from the cache. Otherwise,
## it calculates the inverse and set it into the cache via "setInverse".

cacheSolve <- function(x=matrix(), ...) {
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
    inv
}
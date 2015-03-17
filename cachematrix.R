## These two functions can compute and cache the inverse of a 
## matrix. 

## This function creates a special "matrix" object that can
## 1. set the matrix 
## 2. set the matrix
## 3. cache the inverse of the matrix
## 4. get the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(matr){
        x <<- matr
        inver <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inver <<- inverse
    getInverse <- function() inver
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function will return the inverse of the matrix
## If the inverse has already been calculated, 
## the function will retrieve the inverse from the cache.
## If there is no inverse cached, the function will solve the 
## inverse.

cacheSolve <- function(x, ...) {
    inver <- x$getInverse()
    if (!is.null(inver)) {
        message("getting cached inversed matrix")
        return inver
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setInverse(inver)
    inver
    
}

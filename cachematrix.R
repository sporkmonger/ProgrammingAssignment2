## Creates a wrapped matrix that can memoize its inverse calculation.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Memoized version of the matrix solve(x) function, which caches the result
## of the inverse calculation.
cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }
          data <- x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          inv
}

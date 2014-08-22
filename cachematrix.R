## takes a matrix x 
## calculates the inverse of that matrix 
## creates a function (setinverse) to later cache the inverse as v
## creates a function (getinverse) to show the cached inverse

makeCacheMatrix <- function(x = matrix()) {
     v <- NULL
     set <- function(y) {
          x <<- y
          v <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) v <<- solve
     getinverse <- function() v
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## looks for a stored inverse first
## if finds a stored inverse, returns that matrix, preceeded by message
## calculates inverse if not stored
## caches the calculated matrix

cacheSolve <- function(x, ...) {
     v <- x$getinverse()
     if(!is.null(v)) {
          message("getting cached data")
          return(v)
     }
     data <- x$get()
     v <- solve(data, ...)
     x$setinverse(v)
     v
}

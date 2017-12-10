## This function creates the matrix and crates the assigns values

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse, but if there is an inverse already in cache for a particular matrix then it resolves from cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
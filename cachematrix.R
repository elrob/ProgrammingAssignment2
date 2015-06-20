## makeCacheMatrix and cacheSolve can be combined to 
## store the inverse of the matrix with its data:
##     > m <- makeCacheMatrix(matrix(1:4, 2, 2))
##     > m$get()
##          [,1] [,2]
##     [1,]    1    3
##     [2,]    2    4
##     > cacheSolve(m)
##          [,1] [,2]
##     [1,]   -2  1.5
##     [2,]    1 -0.5


## makeCacheMatrix creates a matrix that caches its inverse 
## when called with the cacheSolve function

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


## cacheSolve calculates the inverse of a matrix created using the
## makeCacheMatrix function and caches the result for future calls

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Use two function to create a cached version matrix

## makeCacheMatrix returns a list of four (two pairs) accessor fuctions.
## The first pair set/get gives access to the original matrix.
## The second pair setinverse/getinverse gives access to the corresponding inverse matrix.
## When the original matrix is set or a new makeCacheMatrix is created, the cached inverse matrix will be reset to NULL.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(n) {
      m <<- n
      i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve is used to calculate the inverse of the cached matrix which is created by makeCacheMatrix.
## If inverse of the cached matrix does not exist (i.e. m$getinverse() is NULL), it will be calculated by solve function.
## Otherwise, the inverse will be returned by the cached result that was calculated previously.

cacheSolve <- function(m, ...) {
  i <- m$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(m$get(), ...)
  m$setinverse(i)
  i
}

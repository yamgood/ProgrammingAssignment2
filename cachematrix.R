# The functions makeCacheMatrix and cacheSolve are used to cache the potentially time-consuming computation of calculating the inverse of a matrix.
# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve retrieves the inverse from the cache.
# Please note, it is assumed that the matrix supplied is always invertible.

# makeCacheMatrix creates a special "matrix" object, which is actually a list containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# cacheSolve calculates the inverse of the special "matrix" created with the
# makeCacheMatrix function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and sets the
# value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}
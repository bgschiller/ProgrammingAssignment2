## cachematrix.R
## provides a function makeCacheMatrix to wrap a matrix,
## and a function cacheSolve to take the inverse of that
## matrix. A cache matrix behaves like a matrix, but caches
## its inverse in order to speed up future computations.

## given a matrix, return a wrapped CacheMatrix with the
## same data
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y #overwrite our internal data
    inv <<- NULL # invalidate the cache 
    #(the new matrix might have a different inverse)
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv)  
}


## return the inverse of a CacheMatrix, either by 
## checking the cached value or by computing the inverse.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() # check the cache first...
  if (!is.null(inv)){
    # success! returning the cached value
    message("getting cached data")
    return(inv)
  }
  # we have to compute the inverse
  data <- x$get()
  inv <- solve(data, ...)

  # store it for next time.
  x$setinv(inv)
  inv
}

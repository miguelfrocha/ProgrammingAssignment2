## Put comments here that give an overall description of what your
## functions do

## This function creates a list with functions to create the cached matrix and its inverse
## each field contains a function: set (sets the matrix), get (returns the matrix), 
## setinv (sets the inverse), getinv (returns the inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(mat)
  {
    m <<- mat
    inv <<- NULL
  }
  get = function() m
  setinv = function(mi) inv <<- mi
  getinv = function() inv
  
  res = list(set = set, get = get, setinv = setinv, getinv = getinv)
  res$set(x)
  res
}


## Computes the inverse: gest from cache if it is computed or computes it, if it is not
## Returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  if (!is.null(inv))
  {
    message ("getting cached data")
    return(inv)
  }
  mat = x$get()
  inv = solve(mat)
  x$setinv(inv)
  inv
}

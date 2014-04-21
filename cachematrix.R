## Put comments here that give an overall description of what your
## functions do

## This function creates a list with functions to create the cached matrix and its inverse
## each field contains a function: set (sets the matrix), get (returns the matrix), 
## setinv (sets the inverse), getinv (returns the inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  ## set function
  set = function(mat)
  {
    m <<- mat
    inv <<- NULL
  }
  # get function
  get = function() m
  # setinv function
  setinv = function(mi) inv <<- mi
  # getinv function
  getinv = function() inv
  
  # builds the list
  res = list(set = set, get = get, setinv = setinv, getinv = getinv)
  # needed to store the matrix when creating cache
  res$set(x)
  # returns the list
  res
}


## Computes the inverse: gest from cache if it is computed or computes it, if it is not
## Returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  ## if inverse is calculated return cached inverse
  if (!is.null(inv))
  {
    message ("getting cached data")
    return(inv)
  }
  ## else compute it and return it
  mat = x$get()
  inv = solve(mat)
  x$setinv(inv)
  inv
}

## example code to run
## m1 = matrix(rnorm(16), 4, 4)
## cm1 = makeCacheMatrix(m1)
## cacheSolve(cm1)
## cacheSolve(cm1)
## second run gets the result from cache

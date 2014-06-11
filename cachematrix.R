## Put comments here that give an overall description of what your
## functions do
##
## Finding the inverse of matricies, especially large nxn matricies is an
## especially time consuming and resource hungry computation.

## The two functions below: makeCacheMatrix and cacheSolve together implement
## a caching strategy, to reduce these potentially time consuming
## computations.

## The only time the actual inverse calculation will be called is subsequent
## to whenever the matrix is initialised.  Consequent calls to get the inverse
## will return a cached value, thereby saving time and resouces.
##
##  Assumptions: Once the matrix is setup it remains unchanged

## Write a short comment describing this function
##
## Setup CacheMatrix vector (object with its attributes and methods).
## We return an object with the following 'methods':
##    - set:  initialise the matrix
##    - get:  return the matrix
##    - setInverse:   caches the given inverse
##    - getInverse:   returns the cached inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
      x <<- y
      inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(
      set = get,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse
  )
  
} # makeCacheMatrix()


## Write a short comment describing this function

## We cache the inverse using this function.  We do this by 1st interrogating
## the cached object x for the inverse.  If null is returned then we know that
## inverse has yet to be calculated and if so, we calculate then cache it
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if ( !is.null(inv) ) {
        message('Cached data returned')
        return(inv)
    }
    
    # We need to calculate and store the inverse
    mat <- x$get()  # get the matrix
    inv <- solve(mat, ...)
    x$setInverse(inv)
    
    inv
    
} # cacheSolve()

## cacheMatrix.R
## Last Edit: 2014.12.19

## These functions store a matrix, and cache its inverse.
## To be used for very large matrices, where the inverse
## is frequently needed and time-consuming to compute.


## This function stores a square matrix and its inverse.
## This function has no error checking: all matrices 
## supplied must be square and invertible.

makeCacheMatrix <- function(x = matrix()) {

  #Initialize inverse matrix to NULL
  xi <- NULL
  
  #Use this function to change the base matrix and re-initialize
  #the inverse matrix to NULL
  set <- function(xNew) {
    x <<- xNew
    xi <<- NULL    
  }
  
  #Return the base matrix
  get <- function() x
  
  #Cache the matrix's inverse as calculated in cacheSolve()
  setInv <- function(inverse) xi <<- inverse
  
  #Return the inverse matrix
  getInv <- function() xi
  
  #Return all 4 functions constructed within this function
  list(set = set, get = get, setInv = setInv, getInv = getInv)  
  
}


## This function checks to see if the matrix inverse has already
## been cached.  If so, it returns the cached value.  Otherwise,
## it calculates the matrix inverse, caches it, and returns it.

cacheSolve <- function(x, ...) {

  #Get the cached value of the inverse
  xi <- x$getInv()
  
  #If the cached value isn't NULL, return the cached value
  if(!is.null(xi)) {
    message("Retrieving cached inverse")
    xi
  }
  
  #Otherwise, calculate the inverse, cache it, then return it.
  else {
    message("Calculating inverse")
    x$setInv(solve(x$get(), ...))
    x$getInv()
  }
  
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  xi <- NULL
  
  set <- function(xNew) {
    x <<- xNew
    xi <<- NULL    
  }
  
  get <- function() x
  setInv <- function(inverse) xi <<- inverse
  getInv <- function() xi
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  xi <- x$getInv()
  if(!is.null(xi)) {
    message("Retrieving cached inverse")
    xi
  }
  else {
    message("Calculating inverse")
    x$setInv(solve(x$get()))
    x$getInv()
    
  }
  
}

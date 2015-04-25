## Put comments here that give an overall description of what your
## functions do

## THis function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
  set <- function(y) {
    m <<- y
    inver <<- NULL
  }
  
  get <- function() m
  set_inver <- function(inverse) inver <<- inverse
  get_inver <- function() inver
  
  list(set=set, get=get, setinverse=set_inver, getinverse=get_inver)
}


## Computes the inverse of the special matrix function returned by the above makeCacheMatrix function.
## cacheSolve should return the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inver <- x$get_inver()
  if(!is.null(inver)) {
    message("Now getting cached data......")
    return(inver)
  }
  dat <- x$get()
  inver <- solve(dat, ...)
  x$set_inver(inver)
  inver
}

## Assignment #2, R Programming Course
## Rich Huebner
## Your assignment is to write a pair of functions that cache the inverse of a matrix



## THis function creates a special matrix object that can cache its inverse


makeCacheMatrix <- function(m = matrix()) {
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
  
  ## This next line includes the solve() function, which computes the inverse of a square matrix.
  inver <- solve(dat, ...)
  
  x$set_inver(inver)
  
  inver

}

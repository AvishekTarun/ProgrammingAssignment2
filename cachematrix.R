## For R Prgromming Course - Programming Assignment 2: Lexical Scoping
##These two functions calculates inverse of a matrix and cache it


## The function "makeCacheMatrix" below creates an object that 
## caches the inverse of the argument matrix

makeCacheMatrix <- function(mtrx = matrix()) {
invert <- NULL
    
  set <- function(a) {
      mtrx <<- a;
      invert <<- NULL;
  }
  
get <- function() mtrx;
setinv <- function(inv) invert <<- inv;
getinv <- function() invert;
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function "cacheSolve" below returns the inverse of the matrix 
## first it checks if the inverse is already computed or not
## If yes, if fetches the result from the cache and skips the inverse cumputatio
## If no, then computes the inverse of the matrix, stores the result in the cache

cacheSolve <- function(mtrx, ...) {
  invert <- mtrx$getinv()
  if(!is.null(invert)) {
    message("Inverse already in cache...fetching")
    invert
  }
  else message("Inverse not in cache...computing")
  data <- mtrx$get()
  invert <- solve(data, ...)
  mtrx$setinv(invert)
  invert
}

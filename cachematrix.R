## These two sets of functions that create an object that stores a 
## matrix and cache's its inverse

## The first function, makeCacheMatrix, creates a list that 1) sets the 
## value of the matrix, 2) gets the value of the matrix, 3) sets the 
## value of the inverse, and 4) gets the value of the inverse.

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The second function, cacheSolve, calculates the inverse of the matrix, 
## but first checks whether it has already been calculated, in which case it 
## skips the computation and retrieves it from the cache instead.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
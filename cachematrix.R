## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  library(MASS)   ## Load MASS for the function ginv to run.
  m<-NULL
  
  set <- function(y){ ##set the value of the matrix.
    x <<- y
    m <<- NULL
  }
  get <- function() x ##get the value of the matrix.
  setinverse <- function(ginv) m <<- ginv ##set the value of the inverse of the matrix.
  getinverse <- function() m  ##get the value of the inverse of the matrix.
  list(set = set, get =get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinverse(m)
  m

        ## Return a matrix that is the inverse of 'x'
}

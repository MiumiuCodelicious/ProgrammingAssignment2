## Cache or compute the inverse of a matrix -- which is usually a costly computation. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y;
    inverse <<- NULL;
  }
  get <- function() return(x);
  setinverse <- function(inv) inverse <<- inv;
  getinverse <- function() return(inverse);
  return( list( set = set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse)
          )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  
  inverse <- solve(data, ...)
  
  x$setinverse(inverse)
  
  return(inverse)
}


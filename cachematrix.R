## makeCacheMatrix function creates a special "matrix" object that can cache the inverse. 
## cacheSolve will computes the inverse of the special “matrix” returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not change then the cachesolve
## will retrieve the inverse from the cache and will compute the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                             
  set <- function(y) {                    
    x <<- y                             
    inv <<- NULL                        
  }
  get <- function() x                     
  
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve will computes the inverse of the special “matrix” returned by makeCacheMatrix.
## In case the inverse has already been calculated then cacheSolve will retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

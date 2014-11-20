## This set of functions allows to create a "special" matrix
## whose inverse can then be cached to avoid repeatedly
## computing it

## makeCacheMatrix creates the "special" matrix that can
## can then be given as an argument to cacheSolve to 
## obtain the inverse


makeCacheMatrix <- function(x = matrix()) {
  ## x is the matrix whose inverse will be cached
  
  ## returns a list that can be given to cacheSolve
  ## to obtain the mean of the matrix with caching
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a "special" matrix returned by 
## makeCacheMatrix and returns its inverse, computing
## it only if it is not already cached

cacheSolve <- function(x, ...) {
  ## x is a "special" matrix as returned by
  ## makeCacheMatrix
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv
}

  
## This function creates a special "matrix" object that can cache its inverse.

makeCachedMatrix <- function(x = Matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(z) inv <<- z
  getInv <- function() inv
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}
## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cachedInverse <- function(x) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    # Already computed
    message("Getting cached data...")
    return(inv)
  }
  message("Computing inverse...")  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}

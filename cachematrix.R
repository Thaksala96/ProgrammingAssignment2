##  These two functions are for cache the inverse of a matrix

## This function creates a special matrix object which can cache itself

makeCacheMatrix <- function(x = matrix()) {
         a <- NULL
    set <- function(y){
      x <<- y
      a <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) a <<- inverse
    getInverse <- function() a 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## This function computes the inverse of the special matrix
#returned by the above function makeCacheMatrix. If the inverse has already
# been calculated then cacheSolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        a <- x$getInverse()
  if(!is.null(a)){
    message("Please wait...Getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}

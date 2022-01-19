## makeCacheMatrix uses scoping to provide get and set methods for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve uses an object returned by makeCacheMatrix to check if the inverse has been cached
## if the inverse has been cached, return that without bothering to recompute
## if it does not exxst, get the original matrix, compute the inverse, and cache it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  
  inv
}

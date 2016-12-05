## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  myinverse <- NULL
  set <- function(k) {
    x <<- k
    myinverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) myinverse <<- inverse
  getInverse <- function() myinverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  myinverse <- x$getInverse()
  if (!is.null(myinverse)) {
    message("getting cached data")
    return(myinverse)
  }
  mat <- x$get()
  myinverse <- solve(mat, ...)
  x$setInverse(myinverse)
  myinverse
}

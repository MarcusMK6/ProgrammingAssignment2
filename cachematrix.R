## The functions computes and caches the invers of a matrix 

## It's creating a matrix and an item in that list that contains inv which is 
## set to NULL. It also creates x and inv which are locally stored in the func.
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <-  NULL
  set <-  function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Checks the created matrix for its inverse, if not computed it passes the
## the message "Getting cached data" and calculates the inverse with the solve()
## function and then printing the inverted matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("Getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv  
}

testmatrix <-  makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

cacheSolve(testmatrix)

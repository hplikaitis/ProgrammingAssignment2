
makeCacheMatrix <- function(x = matrix()) {
  ## Set value of matrix
  inv <- NULL
  set <- function(h){
    x <<- h
    inv <<- NULL
  }
  
  ## Get value of matrix and set and get inverse
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
  
}
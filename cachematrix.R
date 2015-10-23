
## This function creates a list of functions to set/get the value of the matrix and the inverse matrix

makeCacheMatrix <- function(matrix = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    matrix <<- y
    inv_matrix <<- NULL
  }
  get <- function() matrix
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse <- function() inv_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function returns the value of inverse matrix, using the cached value if available, 
##otherwise storing the value in the cache

cacheSolve <- function(matrix, ...) {
  inv_matrix <- matrix$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- matrix$get()
  inv_matrix <- solve(data)
  matrix$setinverse(inv_matrix)
  inv_matrix
}

## Creates the following 2 functions
## 1.) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## Assumption: Matrix supplied is always invertible


## Creates a list that sets the matrix, gets the matrix, sets the inverse of the matrix and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Caches the inverse of the matrix (Assumption: matrix is invertible) created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix 'i' that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i  
}

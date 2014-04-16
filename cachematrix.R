
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
#*     Caching the inverse of a matrix
#* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

# Function to create an object to hold matrix and cache its inverse
# Note: *the supplied matrix must be invertible*
# Usage example:
#       #make a matrix
#       w <- makeCacheMatrix(matrix(rnorm(5*5), nrow=5, ncol=5))
#       cacheSolve(w) #solve for the first time
#       cacheSolve(w) #solve again; returns from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# function to return inverse of the matrix but if the inverse is
#   already calculated, then it retrives the inverse from cache

cacheSolve <- function(x, ...) {
  #return inverse of the matrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retriving cached data") #return message to console
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m #return inverse
}


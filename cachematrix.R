

## The two functions used here help in caching the
## inverse of a matrix. To avoid recomputing the inverse
##  repeatedly, the use of functions computes the result once.

# The function below sets the matrix, gets the matrix,sets the inverse of matrix and finally
##gets the inverse of the matrix
# Initially set to NULL but Changes when the user sets the value

# set function
# Sets the matrix itself but not the inverse then get function is used to get only
## the matrix not the inverse
# Inverse is set and then we get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse<- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Once you create the matrix, one can use the cacheSolve
## function to compute the inverse and cache the result
## If you try using cacheSolve again on the same special
## matrix, then the pre-computed result is obtained, thus
## avoiding any recomputation.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


##Use this to test the function
A3 <- makeCacheMatrix(matrix(4:7, 2, 2))
A3$get()
A3$getInverse()
cacheSolve(A3)
cacheSolve(A3)
A3$getInverse()





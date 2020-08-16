## @JoseMVergara - R Programming - Programming Assigment 2: Lexical Scoping


##Description:  Cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_ <<- inverse
  getinverse <- function() inverse_ 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inverse_ <- x$getinverse()
  if (!is.null(inverse_)) {
    message("getting cached data")
    return(inverse_)
  }
  data <- x$get()
  inverse_ <- solve(data, ...)
  x$setinverse(inverse_)
  inverse_
}

## the following functions will take advantage of the scoping rules of the R language 
## and manipulate them to preserve state inside of an R object.



## This function creates a special "matrix" object that can cache its inverse.
## It creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

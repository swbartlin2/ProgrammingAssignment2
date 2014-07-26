## Below are two functions that are used to create a special object that
## stores a matrix and caches its inverse.


## makeCacheMatrix - This function creates a special 'matrix' object,  
## which contains special functions to: 
## 1: set the value of the matrix
## 2: get the value of the matrix
## 3: set the value of the matrix inverse
## 4: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve - This function looks for a cached value of a matrix, 
## if previously calculated.  If the inverse has not previously been 
## calculated, and therefore not cached, then the function computes
## the inverse and caches it.


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
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


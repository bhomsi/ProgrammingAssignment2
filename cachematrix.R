## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix take argument an object of class matrix
## Define 4 internal functions
## 1. set which map matrix x to the matrix y in the parent environment
## 2. get which retrieve maxtrix x from memory
## 3. setinverse which save matrix in inversed format
## 4. getinverse which retrieve matrix in inversed format
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) m <<- i
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve takes an argument an object of class matrix
## his function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already
##been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
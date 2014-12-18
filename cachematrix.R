## https://github.com/Thovaro/ProgrammingAssignment2.git
## Programming Assignment 2
## The makeCacheMatrix function creates a matrix consisting of the commands:
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set value of the inverse matrix
## 4. get value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
    {x <<- y
    inv <<- NULL}
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)}

## The cacheSolve function calculates the inverse of the special matrix created with the makeCacheMatrix function
## It is first checked whether the inverse has already been calculated and can be skipped
## When not present, the inverse of the matrix is calculated in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("get cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
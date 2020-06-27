## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined herein: setmatrix, getmatrix, setinverse, 
# getinverse and getenv

makeCacheMatrix <- function(x = matrix()) {
  nv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}



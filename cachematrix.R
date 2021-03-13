## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## create a special matrix which can be cached
## call this function first with the matrix to be solved
## returns a special matrix which will be passed into 
## the 'cachesolve' function
## store the returned matrix into a variable

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, 
       get = get, 
       setmatrix = setmatrix, 
       getmatrix = getmatrix)
  
}


## Write a short comment describing this function
## must call 'makeCacheMatrix' function prior to this function
## otherwise you will get and the following error
## Error: $ operator is invalid for atomic vectors
## pass the special matrix returned from the 'makeCacheMatrix' function
## into this function to find the inverse of the initial matrix
## assumes the initial matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  ##m <- x
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}

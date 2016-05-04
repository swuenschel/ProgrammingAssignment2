## makeCacheMatrix: sets up the structore to store a matrix and its inverse
##    also creates the getters/setters for getting the matrices back out
## cacheSolve: inverts the matrix if inverse doesn't already exist

## makeCacheMatris:
## default value of the output matrix as NULL
## setmatrix: sets the input matrix into x and resets defaut to NULL
## get: returns the data currently in the function
## setinv: saves the inverted matrix provided
## getinv: returns the inverted matrix
## list: list of externally accessible functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(val) m <<- val
  getinv <- function() m

  list(setmatrix = setmatrix, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: caches the result of inverting the matrix stored
## in the data container makeCacheMatrix
## input variable x is an instance of makeCacheMatrix
## if cached result does not exist, it does the inversion
## on the matrix stored in x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the one stored in 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }else{
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
  }#end of else
}

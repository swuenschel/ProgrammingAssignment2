## makeCacheMatrix: sets up the functionality to cache an inverted matrix for later use
## cacheSolve: inverts the matrix, if inverse doesn't already exist

## default value of the output matrix as NULL
## setmatrix: sets the input matrix into x and resets m to NULL
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
  ischanged <-function(val){
    !identical(val,x,ignore.environment = TRUE) 
  }
  list(setmatrix = setmatrix, 
       get = get,
       setinv = setinv,
       getinv = getinv,
       ischanged = ischanged)
}


## cacheSolve: caches the result of inverting the function
## if cached result does not exist, it does the inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(x$ischanged(x)) message("new matrix")
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

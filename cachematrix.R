##return a matrix that is the inverse of the argument

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## The process initializes the "inversed" property
  i <- NULL
  
  ## Process to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Process the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Process to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Process to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the processes facts
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix above 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return the previously cached inverse (if it exists)
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the data 
  data <- x$get()
  
  ## Use matrix multiplication to get the inverse 
  m <- solve(data) %*% data
  
  ## Set the inverse
  x$setInverse(m)
  
  ## Print the matrix
}
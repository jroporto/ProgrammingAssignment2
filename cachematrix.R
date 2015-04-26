## ProgrammingAssignment2

## Creates a special "matrix", which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    setInverse the value of the inversed matrix
##    getInverse the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## Calculates the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting inversed matrix")
    return(inverse)
  }
  dataMatrix <- x$get()
  inverse <- solve(dataMatrix)
  x$setInverse(inverse)
  inverse
}

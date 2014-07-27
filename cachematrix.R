## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# special list containing matrix and catched inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  set <- function(y) {
    x <<- y
    invertedMatrix <<- NULL
  }
  get <- function() x
  setInvertedMatrix <- function(inverted) invertedMatrix <<- inverted
  getInvertedMatrix <- function() invertedMatrix
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}


## Write a short comment describing this function
# based on special list object above, calculate inverted matrix if there is no cached inverted matrix found
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvertedMatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  # no cached data, do calculate
  data <- x$get()
  m <- solve(data, ...)
  x$setInvertedMatrix(m)
  m
}

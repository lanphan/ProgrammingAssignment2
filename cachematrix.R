## Demonstration about caching inverted matrix after calculation

# special list structure containing matrix and its cached inverted matrix
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

# based on special list structure above, calculate inverted matrix as followings: 
#  + if found cached inverted matrix, just return
#  + if there is no cached inverted matrix found, do calculation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvertedMatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  # no cached data, calculate inverted matrix based on solve() method
  data <- x$get()
  m <- solve(data, ...)
  x$setInvertedMatrix(m)
  m
}

# Example:
## Step 1: create square matrix x:
##       [,1]   [,2]
## [1,]   4      3
## [2,]   3      2
## Step 2: create special list structure to cache inverted matrix
## Step 3: calculate inverted matrix of x, using cacheSolve() method. Because there is no cache, it'll do calculation
## Step 4: do again step 3, same result is returned. Because it already has cache, result is returned quickly using cache,
##         and  there is additional comment "getting cached data" in result
## The correct inverted matrix of x is:
##       [,1]   [,2]
## [1,]   -2      3
## [2,]   3      -4

# R code:
####  x <- matrix(c(4,3,3,2), 2, 2)
####  xx <- makeCacheMatrix(x)
####  cacheSolve(xx)
####  cacheSolve(xx)   # see additional comment "getting cached data" in result
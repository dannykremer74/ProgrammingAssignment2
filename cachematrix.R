## The following functions expedite processes by creating functions to determine
## if an inverse matrix is cached already or if it needs to calculate it new

## This creates a "matrix-like" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {
    x <<- y
    InvMat <- NULL
  }
  get <- function() x
  setInvMat <- function(inv) InvMat <<- inv
  getInvMat <- function() InvMat
  list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}


## This function attempts to display the inverse matrix from the previous "matrix"
## If no inverse matrix exists, it computes the new inverse matrix and displays it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInvMat()
  if (!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  data <- x$get()
  InvMat <- solve(data, ...)
  x$setInvMat(InvMat)
  InvMat
}

# Testing the code
# m <- matrix(1:4, 2, 2)
# test_matrix <- makeCacheMatrix(m)
# test_matrix$get()
# test_matrix$getInvMat()
# cacheSolve(test_matrix)
# test_matrix$get()
# test_matrix$getInvMat()

## NOTE: I would like to thank Daniele Pigni for the helpful explanation of the
## examples for this assignment.  The explanation can be found at
## https://class.coursera.org/rprog-013/forum/thread?thread_id=694.

## The function makeCacheMatrix defines four functions: set, get, setmatrix, and
## getmatrix. In order to access these functions, one needs to use use the '$'
## symbol as used in subsetting: makeCacheMatrix$get, etc.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

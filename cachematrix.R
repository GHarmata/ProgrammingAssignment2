# NOTE: I would like to thank Daniele Pigni for the helpful explanation of the
# examples for this assignment.  The explanation can be found at
# https://class.coursera.org/rprog-013/forum/thread?thread_id=694.

##############################################################

## Function 1: makeCacheMatrix

## The function makeCacheMatrix defines four functions: set, get, setmatrix, and
## getmatrix. These sub-functions allow the 
## In order to access these functions, one needs to use use the '$'
## symbol as used in subsetting: makeCacheMatrix(x)$get(), etc. 

makeCacheMatrix <- function(x = matrix()) {
  
   #Part 1: Creates an empty variable i.
   i <- NULL
   
   #Part 2: The set, get, setinverse, and getinverse functions
   set <- function(y) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}





## Function 2: cacheSolve

## The function cacheSolve first checks to see if the inverse of a matrix x has
## already been calculated and cached.  If it has, the function retrieves the
## inverse matrix; if not, the function checks to see if the matrix has an
## inverse (det != 0).  If it does, the function calculates the inverse of the
## matrix, caches it for later use, and returns it.

cacheSolve <- function(x, ...) {
   
   # Part 1: Checks if inverse of the matrix has already been cached; if so, it
   # returns the cached inverse
   
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   
   # Part 2: Checks if matrix can be inverted, then solves for the inverse of
   # the matrix if possible.  If the matrix does not have an inverse, a message
   # saying so is returned.  For more information, check out 
   # http://mathworld.wolfram.com/SingularMatrix.html or
   # http://en.wikipedia.org/wiki/Invertible_matrix.
   
   if(det(data)!= 0){
      i <- solve(data, ...)
      x$setinverse(i)
      i
   }
   
   else {
      message("Provided matrix does not have an inverse; determinant = 0")}
}

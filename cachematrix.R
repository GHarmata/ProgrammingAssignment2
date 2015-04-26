## NOTE: I would like to thank Daniele Pigni for the helpful explanation of the
## examples for this assignment.  The explanation can be found at
## https://class.coursera.org/rprog-013/forum/thread?thread_id=694.  The many
## other posts on the forums were also helpful to me in completing this
## assignment, including inspiration for using "####..." dividers. 

##############################################################

## Function 1: makeCacheMatrix

## The function makeCacheMatrix defines four functions: set, get, setmatrix, and
## getmatrix. These sub-functions allow the user to set a matrix or its inverse,
## and to retrieve the previously-cached matrix or previously-cached inverse. 

## TIP: In order to access the sub-functions, one needs to use use the '$' symbol as used in subsetting:
## makeCacheMatrix(x)$get(), etc. The command is less lengthy if one assigns
## makeCacheMatrix(x) to a new variable z in the console, so that one can use
## the command z$get(), etc.

makeCacheMatrix <- function(x = matrix()) {
  
   # Part 1: Creates an empty variable i.
   
   i <- NULL
   
   # Part 2: The set, get, setinverse, and getinverse functions
   
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



###################################################################

## Function 2: cacheSolve

## The function cacheSolve first checks to see if the inverse of a matrix x has
## already been calculated and cached.  If it has, the function retrieves the
## inverse matrix; if not, the function calculates the inverse of the
## matrix, caches it for later use, and displays it. The "m" argument of this
## function should correspond to makeCacheMatrix(x).

cacheSolve <- function(m, ...) {
   
   # Part 1: Checks if inverse of the matrix has already been cached; if so, it
   # returns the cached inverse.
   
   i <- m$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- m$get()
   
   # Part 2: Solves for the inverse of the matrix. This part assumes that the 
   # matrix is invertible, as per the assignment instructions.
   
      i <- solve(data, ...)
      m$setinverse(i)
      i
}



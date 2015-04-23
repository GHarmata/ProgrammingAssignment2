## Example for Assignment 2, taken from assignment 2 description
## Caching the Mean of a Vector, Part 1

## Function: cachemean
## What it does: 
## 1. Looks at the vector created by makeVector
## 2. Checks to see if mean has already been calculated, and:
#### a. If mean has already been calculated, it gets the mean from the cache
#### b. Else it calculates the mean of the vector and stores it in the cache


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

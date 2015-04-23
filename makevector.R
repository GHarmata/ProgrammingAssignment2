## Example for Assignment 2, taken from assignment 2 description
## Caching the Mean of a Vector, Part 1

## Function: makeVector
## What it does: 
## 1. set the value of the vector (the function "set")
## 2. get the value of the vector (the function "get")
## 3. set the value of the mean (the function "setmean")
## 4. get the value of the mean (the function "getmean")



makeVector <- function(x = numeric()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setmean <- function(mean) m <<- mean
   getmean <- function() m
   list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}

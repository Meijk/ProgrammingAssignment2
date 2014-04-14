## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an empty matrix and defines functions to assign values to a list

makeCacheMatrix <- function(x = matrix()) {
# Null vector for the inverse  
  inv <- NULL
# Creating empty variable for matrix
  set <- function(y) {
     x <<- y
     inv <<- NULL
     }
# Call matrix and calculate inverse functions
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
# Store results in a list
  list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
   }
}


## cacheSolve checks for inverse already calculated in the environment, retrieves arguments from makeCacheMatrix
## list and processes solve(x) on data

cacheSolve <- function(x, ...) {
# Call inverse value from the list
    inv <- x$getinverse()
# Check for same result already existing in Environment and output
      if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
     }
# Otherwise retrieve data from list and process inverse, output inv
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
   }

## Author: Adam Ocsvari
## Course: R Programming - via Coursera
## Programming Assignment 2
## Date: 20/06/2015
## Description: Calculating and caching the invers of a matrix. Using the cache insted of recalculation


## makeCacheMatrix contains the helper functions to storing and accessing
## the actual and the stored Matrix values

makeCacheMatrix <- function(x = matrix()) {
  ## internal variable for cached inverse result
  i <- NULL
  
  ## Setter function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Getter function
  get <- function() x
  
  ## Setter function for calculated inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Getter function for calculated inverse
  getinverse <- function() i
  
  ## List of the helper fucntions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is the wrapper function which checks if the input has a cached solution or not.
## Real calculation initiated only if there is no cached value for the actual input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Accessing the cached solution
  m <- x$getinverse()
  
  ## Checking if the cached solution is exists
  if(!is.null(m)) {
    message("getting cached data")
    
    ## Returning the cached solution, no calculation needed
    return(m)
  }
  
  ## Getting the data to do the inverse calculation
  data <- x$get()
  
  ## Calculating the incerse of the matrix
  m <- solve(data, ...)
  
  ## Storing the solution into the cache for later use
  x$setinverse(m)
  
  ## returning the reuslt  
  m
}

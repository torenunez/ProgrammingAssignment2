#---------------------------------------------------------------------------------------------------
# Coursera - John Hopkins Bloomberg School of Public Health 
# R Programming
#---------------------------------------------------------------------------------------------------
# Title:            ProgrammingAssignment2 - makeChacheMatrix and cacheSolve
# Student:          Salvador J Nunez
# Created:          2015-06-19
#---------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------
# makeCacheMatrix
#--------------------------------------------------------------------------

# The first function, makeVector creates a special vector, which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#--------------------------------------------------------------------------
# makeCacheMatrix
#--------------------------------------------------------------------------
# Calculates the inverse of the matrix created with the above function. 
# First checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}


#--------------------------------------------------------------------------
# Testing 
#--------------------------------------------------------------------------
# mat = matrix(rnorm(25),5,5)
# mat
# solve(mat)
# solve(solve(mat))
# z <- makeCacheMatrix(mat)
# cacheSolve(z)

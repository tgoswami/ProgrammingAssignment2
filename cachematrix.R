## Put comments here that give an overall description of what your
## functions do
## Two functions are in the file makeCacheMatrix and cacheSolve to calculate the 
## inverse of the matrix. First function makeCacheMatrix creates a matrix object to cache 
## its inverse
## Write a short comment describing this function
## The function makeCacheMatrix takes one matrix input
## initializes a variable inv to NULL
## There is a set function which caches the matrix
## The inverse is initialized to NULL
## solveMatrix function is called to compute the inverse of the given matrix and 
## store it

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## The makeCacheMatrix returns a special matrix, whose inverse is calculated in this
## function . If the inverse is already calculated the function cachesolve will 
## retrieve the inverse from the cache. solve function is inbuilt R function to 
## compute inverse of a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getInverse()
        if (! is.null(inv))
        {
          message (" Cached inverse matrix is returned")
          return(inv)
        }
        ## not cached , hence compute the inverse
         data <- x$get()
         inv <- solve(data)
         x$setInverse(inv)
         inv
  
}

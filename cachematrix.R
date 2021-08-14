## functions do
## Write a short comment describing this function
##Caches the matrix

makeCacheMatrix <- function(x = matrix()) {
  invertible <- NULL
  set <- function(z) {              ##Sets value of matrix with another function
    x <<- z
    invertible <<- NULL             ##Variables at different levels
  }
  
  get <- function() {x}             ##Gets value of matrix
  setInverse <- function(inverse) {invertible <<- inverse}   ##Sets value of the inverse
  getInverse <- function() {invertible}                      ##Retrieves^
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Write a short comment describing this function
##Gets data from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invertible <- x$getInverse()      ##Gets inverse of x and assigns it to the variable
  if(!is.null(invertible)) {        ##If the inverse was already found, it takes it from the cache
    message("Getting Info In Cache...")
    return(invertible)
   }     
  
  mat <- x$get()                    ##Gets inverse of matrix and sets value, if not done already
  invertible <- solve(mat, ...)
  x$setInverse(invertible)
  invertible                        ##Prints
 }

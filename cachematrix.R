## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix is a special "matrix" object, which is a 
# list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matix

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inve <<- inverse 
  getInverse <- function() inve
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

# The second function returns the inverse of the matrix. 
# If the inverse of matrix has computed, it skips the computation.
# If the inverse of matrix has not computed, it compute the inverse, set the 
# value in the cache through sentinverse function.


cacheSolve <- function(x, ...) {
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("get cached data.")
    return(inve)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inve)
  inve
}

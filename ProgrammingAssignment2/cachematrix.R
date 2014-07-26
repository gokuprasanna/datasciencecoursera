
## Write a short comment describing this function
##  sets up a cache for the matrix and the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse matrix
  i <- NULL 
  ## Assign values to the matrix 
  set <- function(y){
  x <<- y
  i<<- NULL
  }
  ## Return the value of the matrix x
  get <- function()x
  ## Assign values to the inverse matrix
  setinv <- function(inv){
  i<<- inv
  }
  ## Retrieving the inverse matrix
  getinv<-function()i
  ## 
  list(set = set,get = get,
     setinv = setinv, 
     getinv = getinv)
}


## Write a short comment describing this function
## Computes the inverse of the matrix from the cache, if the inverse 
## already exists then it calls the matrix inverse data from the cache.  

cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix 'x'
  i <- x$getinv()
  ## if the inverse is not null then return the inverse matrix
  if(!is.null(i)){
    message("getting cached inverse matrix")
    return(i)
  }
  ## else the matrix is retrieved to compute the inverse
  data <- x$get()
  ## compute the inverse with the solve function
  i <- solve(data,..)
  ## set the inverse of the matrix
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # define the set() function
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # define the get() function
  get <- function() x
  
  # define the setinverse() function
  setinverse <- function() m <<- solve(x)
  
  # define the getinverse() function
  getinverse <- function() m
  
  # return the list of function for the x matrix
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)){
      message("Getting cached data for the matrix")
      return(m)
    }
    
    data <- x$get()
    m <- solve(x)
    x$setinverse(m)
    m
  
}

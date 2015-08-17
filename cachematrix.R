## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # inizialization of the inverse matriz to NULL
  m <- NULL
  
  # define the set() function
  set <- function(y){
    
    # the matrix we are using is now y in the global scope, also
    x <<- y
    # the inverse must be NULL again, as the matrix has changed
    m <<- NULL
  }
  
  # define the get() function (returns the matrix)
  get <- function() x
  
  # define the setinverse() function (estimate the inverse)
  setinverse <- function() m <<- solve(x)
  
  # define the getinverse() function (returns the inverse of the matrix)
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
    
  # check if the inverse is already solved
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data for the matrix")
    return(m) # return the inverse if it is stored in cache
  }
    
  # if not, estimate the inverse of the matrix for the first time
  data <- x$get()
  m <- solve(data)
    
  # for this, we use the setinverse() function
  x$setinverse()
  
  #return the inverse
  m
  
}

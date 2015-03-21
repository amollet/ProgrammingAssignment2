## Functions to cache the inverse of a matrix in a special "matrix" object

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL ## Initialisation
  set <-function(y) ## Store the value of the matrix
  {
    x<<-y
    inv<<-NULL #Reset the inverse 
  }
  
  ## Get the value of the matrix
  get <-function() x 
  
  ## Store the value of the inverse
  setinv <-function(inverse) inv<<-inverse
  
  ## Get the value of the inverse (NULL if cacheSolve function has never been called)
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Look for the value of the inverse in the cache
  inv <- x$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)    
  }
  
  ## If the cache is empty calculate the inverse
  data<- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv  
}

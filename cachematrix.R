
## this function creates an CacheMatrix object from a invertable matrix
makeCacheMatrix <- function(x = matrix()) {

  #setting inverse to NULL
  inverse <- NULL

  #declaring the set function of the cacheMatrix
  set <- function(y){
    #setting the x values
    x <<- y
    #setting the inverse to NULL
    inverse <- NULL
  }
  
  #declaring the get function of the cacheMatrix
  get <- function() x
  #declaring the setInverse function of the cacheMatrix
  setInverse <- function(inv) inverse <<- inv
  #declaring the getInverse function of the cacheMatrix
  getInverse <- function() inverse

  #returning the completed CacheMatrix object
  list(set =set , get = get, setInverse = setInverse , getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  #checks if there is cached inverse matrix available
  if(!is.null(inverse))
  {
    message("Getting cached data...")
    #return inverse matrix if available and quit current function
    return(inverse)
  }
  else
  {
    #if no cached inverse matrix is available
    #calculate and save inverse to cache
    message("Calculating inverse matrix...")
    
    #get the matrix
    data <- x$get()
    #invert the matrix
    inverse <- solve(data)
    #save inverted matrix to cache
    x$setInverse(inverse)
    #return inverted matrix
    inverse
  }
 
}

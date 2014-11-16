## Make matrix returns a list of functions to store a given matrix 
## and associated functions to find its inverse matrix. 
## The inverse matrix is calculated for the first time and stored(cached) for later use. 
## Subsequently its inverse is not calculated, instead it is read from the cache.

makeMatrix <- function(mat = matrix()) 
{
  ## inv - used to store inverse of the input matrix  
  inv <- NULL
  
  set <- function(y)
  {
	mat <<- y
	inv <<- NULL
  }
  
  get <- function() 
  {
	mat
  }
  
  setInv <- function(inverse) 
  {
	inv <<- inverse
  }
  
  getInv <- function()
  {
	inv
  }
  interface <- list(set = set, 
                    get = get,
                    setInv = setInv,
                    getInv = getInv)
  interface
}


## This function returns the inverse of an input matrix x.
## If the inverse has already been cached in x the cached inverse is returned, else inverse is calculated here.
## R function solve is used to calculate the inverse of a square matrix.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x
	  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## These two functions work together to make a particular calculation more efficient

## makeCacheMatrix creates a cache in which a matrix inverse can be stored
makeCacheMatrix <- function(x = matrix()) {
  #initialize matrix var
  m <- NULL
  #spec set function .. sets x value and places it outside of function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #spec get function .. gets x value
  get <- function() x
  #spec setinverse sub function and store outside of function
  setinverse <- function(inverse) m <<- inverse
  #spec getinverse sub function
  getinverse <- function() m
  #output a list of sub functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cachSolve fetches the stored inverse if available, else calculates it
cacheSolve <- function(x, ...) {
  #fetch from cache
  m <- x$getinverse()
  #if inverse is already cached, use that
  if(!is.null(m)) {
    message("getting cached data.")
    #this next line returns value and stops function
    return(m)
  }
  
  #if inverse not cached then get matrix to solve
  data <- x$get()
  #calculate the inverse of the matrix 
  m <- solve(data)
  #cache that inverse
  x$setinverse(m)
  #return inverse of matrix
  m
}

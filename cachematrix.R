## makeCacheMatrix creates a special matrix, which is really a list 
## containing a function to set the value of the matrix, 
## get the value of the matrix, set the value of the inverse, and
## get the value of the inverse.

## cacheSolve calculates the inverse of the special matrix created 
## with makeCacheMatrix function. It first checks if the inverse
## has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the 
## inverse and sets the value of the inverse in the cache via 
## setinverse function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #m is a cache storing the inverse of matrix x
  
  #set is a function to specify the value of matrix x
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #get is a function that simply returns the matrix x
  get <- function() x
  
  #setinverse is a function that stores the inverse of matrix x to the cache m
  setinverse <- function(inverse) m <<- inverse
  
  #getinverse is a function that returns the inverse of matrix x from cache m 
  getinverse <- function() m
  
  #the return value of this function, i.e., a special matrix which is actually a list of functions
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  #If the special matrix has already contained its inverse in its cache, returns it
  if(!is.null(m)){
    message('getting cached data')
    return (m)
  }
  
  #The special matrix does not have its inverse in the cache yet, 
  #thus compute the inverse and store it in the cache 
  data <- x$get
  m <- solve(data)
  x$setinverse(m)
  m
}

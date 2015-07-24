## This pair of functions work together to cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) { ## This function creates a special "matrix" object that can ##cache its inverse
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL      
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() m
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
    
  }
  
  
## The cacheSolve function computes the inverse of the matrix 
##returned by makeCacheMatrix(). If the inverse has already been calculated 
##(and the matrix has not changed),then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #instructs R to assign value from function above to i
  if(!is.null(i)) { #where there is an existing value return it
      message("getting cached data")
      return(i)
  }
  data <- x$get() #or else seek new value
  i <- solve(data, ...)
  x$setinverse(i)
  i #and print it
  
}


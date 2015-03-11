## The following 2 functions return the inverse of a matrix either from cache (if the inverse of the
## matrix is already calculated) or gets calculated freshly. Imp: while sending the argument to the
## first function, please send a nXn matrix or in other words, please send a square matrix


## The first function, makeCacheMatrix creates a list containing a function to
## set the value of the Matrix, get the value of the matrix,
## set the inverse of the matrix, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <-NULL
set <- function(y){
  x <<- y
  m <<- NULL
}

get <- function() x
setinverse <- function(solve) m <<- solve
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

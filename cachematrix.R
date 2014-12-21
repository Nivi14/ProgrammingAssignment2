## Put comments here that give an overall description of what your
## functions do
## The function makeCacheMatrix 1)sets the matrix 2) gets matrix 3) sets inverse 
## 4) gets inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The function cacheSolve takes the matrix created by makeCacheMatrix, checks  
## if the inverse of this matrix has already been calculated. If so, it gets
## the inverse from the cache,if not , it calculates the inverse of the matrix
## and sets the value of inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  x$set(y)
  m <- solve(data)
  x$setinverse(m)
  m
        
}

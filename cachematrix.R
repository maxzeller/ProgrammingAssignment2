# Creates a special function  to cache or get its argument or its inverse (if already available)

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

# cacheSolve calculates the inverse of the"result-matrix" created with the above 
# function. It uses cached result if it is available or coputes it if not already available.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#Example how to use
#================
test<- makeCacheMatrix(matrix(1:4,2, 2)) 
cacheSolve(test) #calculation performed because not yet cached
cacheSolve(test) # no calculation performed because result already cached reused


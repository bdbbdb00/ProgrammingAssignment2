## Matrix inversion with cache - week 3 HW

#Create matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinv <- function(solve) inv <<- inv
getinv <- function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
#print(getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

test<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(example)

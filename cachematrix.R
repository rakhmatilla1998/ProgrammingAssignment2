## makeCacheMatrix function has 4 additional functions and 2 variables
## It is designed to create a matrix with makeCacheMatrix function as well as 
## with set function. get function extracts the value of a matrix, e.i. variable x
## setinv and getinv are designed to set the value for inverse matrix and extract that 
## value


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This functions returns a matrix that is the inverse of 'x'. It first checks 
## whether matrix x has already identified inverse. If yes, then it simply extracts that
## value, otherwise it calculates the inverse for matrix x

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

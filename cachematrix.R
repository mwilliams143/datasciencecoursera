## The function will set a value of a matrix and then get it
## it will then set the value of the matrix inverse and then get it
## when called - if the inverse of the matrix exists in the cache
## it will be retreived from there and print out to screen as such
## if the matrix inverse does not exist in cache, it will compute it

## Set and get a matrix
## set and get a matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

## Return a matrix that is the inverse of 'x'
## taken from cache if it's avalable otherwise
## calculated if it is not available in cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

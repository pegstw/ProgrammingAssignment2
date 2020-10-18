##makeCacheMatrix sets and gets the matrix first 
##later sets and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() {invmat}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve checks to see whether the inv has been computed
##if yes provides cached solution else solves for matrix

cacheSolve <- function(x, ...) {
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
  return(invmat)
    }
  matx <- x$get()
  invmat <- solve(matx, ...)
  x$setinv(invmat)
  invmat
}


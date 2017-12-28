##Functions makeCacheMatrix(mx) and cacheSolve(mx) calculate and
##cache results of an inverse operation of Matrix mx


## makeCacheMatrix returns a list of functions allowing storage
## and return of a Matrix and its Inverse.
## Usage: 
  ## myMx <- matrix(c(5,5,3,3), 2, 2)
  ## myCachedMx <- makeCacheMatrix(myMx)

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(solveddata) invs <<- solveddata
  getinvs <- function() invs
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## cacheSolve calls the makeCacheMatrix function, and returns its
## inverse if previously calculated. If it has not previously been
## calculated, it calculates the inverse and stores it using the 
## makeCacheMatrix function.
## Usage: (run above example first)
  ## cacheSolve(myCachedMx)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvs()
    if(!is.null(inv)) {
      message("returning cache")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinvs(inv)
    inv
}

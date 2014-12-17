## caching the inverse of a matrix

## The "makeMatrix" function creates a special matrix object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinvs <- function(invs) i <<- invs
    getinvs <- function() i
    list(set = set, get = get, 
         setinvs = setinvs, getinvs = getinvs)
 
}


## "cacheSolve" function calculates the inverse of the special matrix created by "makeMatrix" above. 
## If the inverse has already been calculated, then"cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
    i<- x$getinvs()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i<- invs(data, ...)
    x$setinvs(i)
    i

}

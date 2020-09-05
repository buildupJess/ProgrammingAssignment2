## makeCacheMatrix takes a matrix and sets up the matrix to have the inverse
## stored in the cache. cacheSolve returns the inverse  if it exists or
## inverts the matrix if the inverse does not already exist.

## Sets up a matrix object (assume square and invertible) and caches the inverse
## but does NOT compute the inverse

makeCacheMatrix <- function(x = matrix()) {
  #initializes the inverse in case cacheSolve has not been called yet
  inv <- NULL
  #reset the x matrix so makeCacheMatrix does not have to be called completely
  #over again when a new matrix is wanted
  set <- function(newmatrix) {
    x <<- newmatrix
    inv <<- NULL
  }
  #pull the value of the x matrix
  get <- function() x
  #set a new inverse value to the setinv attribute of x
  setinv <- function(newinv) inv <<- newinv
  #get the inverse value of the getinv attribute of x
  getinv <- function() inv
  
  #return a list that has attributes set, get, setinv, and getinv that 
  #match with their respective function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Uses the matrix from makeCacheMatrix and computes the inverse
## ONLY if the inverse does not exist. If it computes, it caches this inverse.
## If inverse exists, cacheSolve pulls the inverse from the cache.

cacheSolve <- function(x, ...) {
  #pull inverse from makeCacheMatrix
  inv <- x$getinv()
  #If inverse has been computed and stored, return this value.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Else (if previous return did not happen), solve inverse and cache.
  #first get matrix
  matrix <- x$get()
  #then calculate inverse
  inv <- solve(matrix,...)
  #set the inverse to the matrix attribute setinv
  x$setinv(inv)
  #return the inverse
  inv
}

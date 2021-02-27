## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function. The makeCacheMatrix create a special "vector"
##, which is really a list containing a function to

##set the value of the vector
##get the value of the vector
##set the old value of the vector
##get the old value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  Inv <-  matrix(, nrow = nrow(x), ncol = ncol(x))
  M_old <-  matrix(, nrow = nrow(x), ncol = ncol(x))
  set <- function(y=matrix()) {
    x <<- y
    Inv <<- matrix(, nrow = nrow(y), ncol = ncol(y))
  }
  set_M_old <- function(y=matrix()) {
    M_old <<- y
  }
  get <- function() x
  get_M_old <- function() M_old
  setInv <- function(Matr= matrix()) {Inv <<- Matr}
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



## Write a short comment describing this function.The cacheSolve function computes the inverse
##of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    data <- x$get()
    Inv <- x$getInv()
    M_old<-get_M_old()
  if(!all(is.na(Inv))&& identical(M_old,data)) {
    message("getting cached data")
    return(Inv)
  }
  
  Inv <- solve(data, ...)
  x$setInv(Inv)
  x$set_M_old(data)
  Inv
  ## Return a matrix that is the inverse of 'x'
}
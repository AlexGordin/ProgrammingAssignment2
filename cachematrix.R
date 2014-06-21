##########################################################################################################################
## This program defines 2 functions - makeCacheMatrix() and cacheSolve() used to get and cache inverse of matrix.
## Usage example:
## [Step 1] is to create a vector "start" as a list of functions get(),set(),getSolve() and setSolve().
## > start <- makeCacheMatrix()
## > start
## $set
## function (y) 
## {
##   x <<- y
##   s <<- NULL
## }
## <environment: 0x00000000103b4fd8>
##   
##   $get
## function () 
##   x
## <environment: 0x00000000103b4fd8>
##   
##   $setSolve
## function (solve) 
##   s <<- solve
## <environment: 0x00000000103b4fd8>
##   
##   $getSolve
## function () 
##   s
## <environment: 0x00000000103b4fd8>
##
## [Step 2] is to use set() function to set a matrix. We will use get() function to make sure set() worked correctly.
## > start$set(matrix(1:4,2,2))
## > start$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## [Step 3] is to get inverse of our matrix using cacheSolve() function. Please note that first attemt to use 
##          cacheSolve() on new matrix will actually solve, but other attempts will retrive cached result (you'll 
##          get "[INFO] Getting cached data" note)
## > cacheSolve(start)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(start)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##########################################################################################################################

##########################################################################################################################
## This function create a list of 4 functions(get(),set(),getSolve() and setSolve()) to be used with cacheSolve().
### set() used to set "x" as new matrix and "s" as NULL
### setSolve() used to store "s" - "x"'s inverse
### get() used to return "x"
### getSolve() used to return "s" - "x"'s inverse
##########################################################################################################################

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}
##########################################################################################################################

##########################################################################################################################
## This function gets a matrix as an argument and calculate inverse matrix or gets inverse matrix from cache
##########################################################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("[INFO] Getting cached data")
    return(s)
  }
  message("[WARN] No cache data exist. May take some time to calculate ...")
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}
##########################################################################################################################

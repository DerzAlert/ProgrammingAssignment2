## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  # set function
  set <- function(y) { 
    x <<- y
    x.inv <<- NULL
}

  #get function
  get <-function()
    x
  
  #SetInv Function
  setInv <- function(y.inv) {
    x.inv <-- y.inv}
  
  #Getinv function
  getInv <- function()
    x.inv
  
  #return list of set and get functions
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv,
  )
}
  )
  

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        x.inv <-x$getInv()
        if (is.null(x.inv)) {
          x.inv <- solve(x$get(),...)
          x$setInv(x.inv)
        }
        else
          message ("getting cached data")
        x.inv
}


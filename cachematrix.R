##Assignment2: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {     ## set matrix value
    x <<- y
    m <<- NULL
  }
  get <- function() x      ## get matrix value
  setinverse <- function(solve) m <<- solve   ## set inverse matrix value
  getinverse <- function() m       ## get inverse matrix value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()    ## read value from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## no values in cashe
  data <- x$get()        ## get matrix value   
  m <- solve(data, ...)  ## inverse matrix
  x$setinverse(m)        ## set inverse matrix value
  m   
}


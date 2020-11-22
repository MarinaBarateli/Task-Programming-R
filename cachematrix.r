## This function creates a special "matrix" object that can cache its inverse.
## The function is created, which can reproduce the matrix, store it's content and have an
## inverse option. x - variable of the martrix, m - inverse. Set - function of correct z (argument)
## inside it

## <<- used in functions, and cause a search to be made through 
## parent environments for an existing definition of the variable being assigned

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve`: This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("cached data output")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

## Check of the solution for the next 3 commands:
mat <- makeCacheMatrix()

## Introduce the matrix form
mat$set(matrix(data = (1:12), nrow = 4, ncol = 3))

## Output
mat$get()
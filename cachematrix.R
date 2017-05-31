## Overall Description:
  # In simple words, my functions cache the inverse of a matrix.
  # The two functions work together to create an object that stores a matrix and then cache's its inverse.
  # The inverse is pulled from the cache if the inverse has already been calculated. If not, however, the second function
  #   calculates the inverse of the matrix and uses the setinverse function to set the inverse in the cache.

## makeCacheMatrix Description:
  # This function creates an object used to create (set) the value of the matrix, pull (get) the value
  #   of the matrix, create the inverse of the matrix, and then pull the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  create <- function(y) {
    x <<- y
    m <<- NULL
  }
  pull <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(create = create, pull = pull,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve Description:
  # This function uses the above function (makeCacheMatrix) to calculate the inverse of the specific object (the given
  #   matrix).
  # To save on time-consuming computations, the function first checks to see if the inverse has already
  #   been calculated for the given matrix. If so, the inverse is pulled from the cache. Thus, skipping the
  #   computation.
  # If the inverse has not yet been calculated for the given matrix, the inverse is then calculated. After this,
  #   the inverse value is then placed in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting chached matrix inverse")
    return(m)
  }
  matrixinv <- x$pull()
  m <- solve(matrixinv,...)
  x$setinverse(m)
  m
}
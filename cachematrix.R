# The following two functions are used to create a invertible matrix
# and make the inverse of the matrix available in the cache environment
# if the matrix has been cached before



# function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL  #set a counter to null 
      set <- function(y) {
         x <<- y 
         m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# function cacheSolve calculates the inverse of the special matrix created with the above function
# it first checks to see if the inverse of the matrix has already been calculated
# if it has been already calculated it it gets the inverse from the cache 
# otherwise it calculates the inverse of the data and sets the inverse matrix in the cache via the setinverse function
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
         message("getting cached data")
         return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}




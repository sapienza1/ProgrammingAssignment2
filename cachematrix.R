## The first function, makeCacheMatrix creates the matrix, 
## which is really a list containing functions to

##  *set the value of the matrix
##  *get the value of the matrix
##  *set the value of the inverse
##  *get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {
      	x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
		setinverse = setinverse,
          	getinverse = getinverse)
}



## This second function allows to get the inverse of a matrix,
## returning a cached valued if available

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
      if(!is.null(m)) {
      	message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      m
}














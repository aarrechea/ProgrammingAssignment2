## This functions creates a catch an inverse of a matriz in order to avoid delay
## in the computational time if the calculation has to be done over the same matrix



## This function generates a list of four functions that not only allow to get and set
## the matrix, but to calculate the inverse of it, and get the result.
makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      
      set <- function(y) 
      {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      setInverse <- function(solve) m <<- solve
      
      getInverse <- function() m
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function search for the inverse matrix first, if there is none, 
## use the function define previously to calculate the inverse. After that
## show the result.
cacheSolve <- function(x, ...) {
        
      m <- x$getInverse()
      
      if(!is.null(m)) 
      {
            message("getting cached data")
            return(m)
      }
      
      data <- x$get()
      
      m <- solve(data, ...)
      
      x$setInverse(m)
      
      m
}

matriz <- matrix(rnorm(4), nrow = 2, ncol = 2)
matriz_especial <- makeCacheMatrix(matriz)
matriz_invertida <- cacheSolve(matriz_especial)
matriz_invertida











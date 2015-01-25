## These functions allow for a matrix to be "cached" - the makeChacheMatrix
## into a "Special Matrix" which not only holds its value but also its inverse
## -a computational intensive task- outside the global environment

## The makeCacheMatrix accepts a matrix as a formal, initialises the inverse 
## value to NULL and then provides a number of closures to access the matrix
## (the "get" functions) and update it's contents (the "set" functions). These 
## tasks are further defined in a list so that they can be easily mapped 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y = matrix()) {
            
            x <<- y
            inv <<- NULL
            
      }
      
      get <- function() x
      setinverse <- function(inverse = matrix()) inv <<- inverse
      getinverse <- function () inv
      list( set = set, get = get, 
            setinverse = setinverse,
            getinverse = getinverse)
}


## The cacheSolve function accepts as an argument a "Special Matrix" 
## as defined above. It checks to see if the inverse of the matrix has 
## already been defined and if this not the case it will proceed to compute
## it and store it with the "setinverse" function of the special cached matrix

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
       
            message("getting the inverse cached data")
            return(inv)
            
      }
      data <- x$get()
      inv <- solve(data, ... )
      x$setinverse(inv)
      
      inv
         
}

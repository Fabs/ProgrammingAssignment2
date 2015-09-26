##This file contains a set of functions to cache the computation necessary to INVERT a matrix,
##If you use these functions you will only have to make the computation once, and store it for future use.

##The code bellow expects the given matrix inverse to be COMPUTABLE, if for any reason you pass
##a matrix with no inverse (e.g. linear dependent columns) the calculation will throw an Error.

##We call the list() returned by makeCacheMatrix, the cacheMatrix object, or cm

##Usage
##mx = matrix(c(4,3,3,2), nrow=2, ncol=2)
##cm <-makeCacheMatrix(mx)
##cacheSolve(cm)
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##cm$get() %*% cm$getinverse()
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    
##cm$get() %*% cacheSolve(cm)
##getting cached data
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1


##makeCacheMatrix returns an "object" cachedMatrix that encapsulates a matrix into a clojure
##cachedMatrix$get -> returns the said matrix
##cachedMatrix$set(M) -> sets the internal matrix to a new matrix M
##cachedMatrix$setinverse() -> 
##cachedMatrix$getinverse() -> retrieves the inverted matrix, or NULL if calculation not performed yet
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_to_store) inv <<- inverse_to_store
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve retrieves the inverse of a matrix given an "object" created using the makeChacheMatrix function.
##if the inverse already exists, it returns it, if it does not yet exists, it computes it only once and stores
##it on the cacheMatrix "object"
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

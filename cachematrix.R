## Below are two functions that are used to create a special object that 
## stores a square matrix and caches its inverse. 
## 
##The first function, makeCacheMatrix, creates a special square "matrix", which caches 
## its inverse. It is really a list containing a function to
##
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the matrix inverse
## 4.get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    x <<- y
    inv <<- NULL   #initialize the cache for the inverse
    }
    get <- function() x
    setinverse <- function(inverse) inv<<- inverse  ##stores inverse in cache
    getinverse <- function() inv  ## gets inverse from cache
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    }
##
## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function.
##


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {        # If cache is not NULL
    message("getting cached data")  #skip calculating the inverse
    return(inv)    # return the inverse in cache
  }
  data <- x$get()
  inv <- solve(data, ...)  #Otherwise calculate its inverse
  x$setinverse(inv)   # update the inverse in cache
  inv
  
}

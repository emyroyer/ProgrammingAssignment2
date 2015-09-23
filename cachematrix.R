## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.
#cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
#If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.


## Write a short comment describing this function
#makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

      ## Initialize the variable inv to NULL so we can tell when cacheSolve has run at least once.
      inv <- NULL
      
      ## Create the set function to store the matrix passed in the call as x and NULL as inv, both in cache.
      set <- function(y) {
            ## Put the initial matrix from the command line into cache as cache_x
            x <<- y
            ## Initialize inv to NULL so we can tell when cacheSolve has run at least once
            inv <<- NULL
            
            ## Create function to get/return the matrix passed in the command line call to '$set
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            ## Create function to retrieve value of inv from cache and return inv to the caller so we can check it for NULL
            getinverse <- function() inv
            
            list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
      }
      
}


## Write a short comment describing this function
#cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
#If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.

# cacheSolve returns the inverted form of the submitted matrix.
# When cacheSolve is called, cacheSolve checks to see if there already exists a non-NULL value for a in cache. 
# If cacheSolve finds a non-NULL value for a existing in cache already, it returns that value.  
# If cacheSolve does not find an existing non-NULL value for a in cache, cacheSolve gets the commandline values for a, inverts the matrix 
# in a, and sets the value of a in the cache environment to the just-computed inverted matrix.
# cacheSolve then evaluates the ending matrix so as to return it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      ## Get the value for a in the cache environment and put it in a local a.
      inv <- x$getinverse()
      
      ## Check to see if a is NULL
      ## If a is not NULL, return the value of a with a message
      if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
      ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to data                         
      data <- x$get()
      ## Use solve() to invert the data. Assign the result to inv.
      inv <- solve(data)
      ## Call nested function x$setinverse() in makeCacheMatrix to set a in the cache environment to the local non-NULL inverted result in inv
      x$setinverse(inv)
      ## Evaluate inv so as to return it to caller/console if inv is non NULL
      inv
      
}

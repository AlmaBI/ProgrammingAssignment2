



## This function creates a list of funcions that cache th inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
 im <- NULL
   set <- function(y){
       x <<- y
           im <<- NULL
	     }
	       
   get <- function() x
   setInverse <- function(inverse) im <<- inverse
   getInverse <- function() im
		       
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Computes the inverse of the matrix returned if not already done.

cacheSolve <- function(x, ...) {
       im <- x$getInverse()
         if(!is.null(im)) {
           message("getting cached matrix")
           return(im)
         }
        mat <- x$get()
        invMat <- solve(mat, ...)
        x$setInverse(invMat)
        invMat 
}

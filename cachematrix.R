## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to matrix 
## inversion that we will not discuss here). I write a pair of functions 
## that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # declaration of variables
  inv <- NULL;
  
  # set the value of the matrix
  set <- function(y){
    x <<- y;
    inv <<- NULL;
  }
  
  # get the value of the matrix
  get <- function() x;
  
  # set the value of the inverse matrix
  setinv <- function(inverse) inv <<- inverse ;
  
  # get the value of the inverse matrix
  getinv <- function() inv;
  
  # return  the result
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv();
  if(!is.null(inv)){
    message("getting cached data");
    return(inv);
  }
  data <- x$get();
  inv <- solve(data,...);
  x$setinv(inv);
  
  ## Return a matrix that is the inverse of 'x'
  inv;
}

## The functions below allow retrieving the inverse of a matrix from a cache
## such that the computation of the inverse needs to be performed only once. 

## Usage example:

# 
# my_matrix <- matrix(data=1:4, 2, 2)   ## create your favorite matrix
# cm <- makeCacheMatrix(my_matrix)      ## create a matrix wrapper that can cache the inverse
# cacheSolve(cm)    ## get the inverse of my_matrix computed    
# cacheSolve(cm)    ## get the inverse of my_matrix from cache              
# cm$get()          ## get your original matrix "my_matrix"
# another_matrix <- matrix(data=2:5, 2, 2)  ## a different matrix
# cm$set(another_matrix)  ## set the new matrix into the wrapper 
# cacheSolve(cm)    ## get the inverse of another_matrix computed    
# cacheSolve(cm)    ## get the inverse of another_matrix from cache              
# 

## This function creates a special "matrix" object that can cache its inverse.
# Arguments:
# x : your input matrix (must be invertible)

makeCacheMatrix <- function(x = matrix()) {
  # m caches the matrix inverse 
  m <- NULL               
  
  # set() is an alternative to calling makeCacheMatrix() again
  set <- function(y) {    
    x <<- y
    m <<- NULL
  }
  
  # x is a free variable defined in the scope of makeCacheMatrix(x). This function captures the argument x for later use.
  get <- function() x     
  
  # function to set the inverse to the cache
  setinverse <- function(inverse) m <<- inverse
  
  # function to get the inverse from the cache
  getinverse <- function() m
  
  # return a list of function objects. These will be used by cacheSolve().
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
# Arguments:
# x : The object returned by makeCacheMatrix()
# ... :   Any other arguments are passed into the wrapped solve() function.
#         These arguments must be identical each time the function is invoked
#         in order for the caching mechanism to work correctly.

cacheSolve <- function(x, ...) {
  # first check the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    # cache is filled
    message("getting cached data")
    return(m)
  }
  # cache is empty, now get the data, compute the inverse, and set it to the cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  # return the matrix that is the inverse of the 'x' argument passed into makeCacheMatrix(x)
  m          
}


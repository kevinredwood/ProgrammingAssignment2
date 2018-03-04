## Programming Assignment 2: Lexical Scoping
## makeCacheMatrix and cacheSolve

## makeCacheMatrix is a function which creates a "matrix" object that can
## cache its inverse for the input 

makeCacheMatrix <- function(x = matrix()) {
+  inv <- NULL
+  set <- function(y) {
+  x <<- y
+  inv <<- NULL
+  }
+  get <- function() x
+  setinv <- function(inverse) inv <<- inverse
+  getinv <- function() inv
+  list(set = set, get = get, setinv = setinv, getinv = getinv)
 }	 

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## , then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) 
        ## Return a matrix that is the inverse of 'x'
+  inv <- x$getinv()
+  if(!is.null(inv)) {
+  message("getting cached result")
+  return(inv)
+  }
+  data <- x$get()
+  inv <- solve(data, ...)
+  x$setinv(inv)
+  inv
       

## m <- matrix(rnorm(16),4,4)
## m1 <- makeCacheMatrix(m)
## cacheSolve(m1)

   [,1]       [,2]        [,3]      [,4]
[1,] -0.7200088  0.4337148 -0.54928935 -2.063104
[2,] -0.8449958  0.9170020  0.06529045 -1.539073
[3,] -1.3274447  0.8576525 -1.15803896 -1.488902
[4,]  0.4891410 -0.8372395  0.68270525  1.289308

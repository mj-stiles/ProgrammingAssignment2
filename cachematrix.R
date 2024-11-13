# The following code is to find the inverse of a matrix
# It checks the cache to see if the inverse has already been calculated
# I followed the same structure the "cacheMean" function from the example

makeCacheMatrix <- function(x = matrix()) {
  #the function creates a matrix but
  #I could not make it work before first creating a matrix
  #I used matrix() to create a matrix and named it my_matrix
  #I then used makeCacheMatrix(my_matrix)
inv <- NULL #This sets the inv as a null (initial value)
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinv <- function(solve) inv <<- solve
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# This function calculates the inverse of the matrix 'x'
# The matrix 'x' was stored using the function makeCacheMatrix
#If the inverse is already calculated, it will get it
#If not then it will calculate it

cacheSolve <- function(x, ...) {
       inv <-x$getinv()
       if(!is.null(inv)) {
         message("getting cached data") 
         return(inv) ## Return a matrix that is the inverse of 'x'
       }
       data <- x$get()
       inv <-solve(data,...)
       x$setinv(inv)
       inv
}


## This function creates a special type of matrix. It actually consists on a list of 
##         functions for manipulating a matrix. It has four fuunctions:
## The 'set' function sets the values of the matrix and NULL for the inverse, so each time a matrix is
##         created or changed, it's inverse is reset to NULL and should be calculated.
## The 'get' function returns the value of the matrix.
## The 'setinv' function is used to set the value of the inverse to be stored in cache, so once it is 
##         calculated, the is no need to calculate it again (until the matrix is 'set' again)
## The 'getinv' function returns the value of the inverse (it will be NULL or the actual inverse if it
##         has been already calculated (using 'setinv')

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a the special type of matrix created with the function 
##         'makeCacheMatrix'. The inverse is only calculated if it has not been previously calculated,
##         otherwise it returns the inverse from the cache. The usage could be as follows:
##     myMat <- rbind(c(m11, m12), c(m21, m22)) ## Should be invertible
##     mySpecialMat <- makeCacheMatrix(myMat)
##     cacheSolve(mySpecialMat) ##This calculates the inverse, since it has not been calculated yet.
##     ##other code here not using 'mySpecialMat <- makeCacheMatrix()' again.
##     cacheSolve(mySpecialMat) ##This returns the inverse from cache, since it has been already calculated.

cacheSolve <- function(x, ...) {
          inv <- x$getinv() ## gets the inverse with the 'getinv' function
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv) ## if !NULL then it has been already calculated. Return it from cache and exit
          }
          message("calculating matrix inverse")
          matrix <- x$get() ## if the inverse has not been calculated, 
          inv <- solve(matrix)
          x$setinv(inv)
          inv
}

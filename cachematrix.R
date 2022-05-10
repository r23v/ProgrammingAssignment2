## makeCacheMatrix`:creates a special "matrix" object
##that can cache its inverse.
##2.  `cacheSolve`: computes the inverse of the special
"##matrix" ##object created above . If the inverse has
##already been calculated, it retrieves the inverse from the cache- 
##with the mesage "getting cached data".



makeCacheMatrix <- function(x = matrix()) {
  ##creates a matrix and an empty vector mt
    mt <- NULL
    set <- function(y) {
      x <<- y
      mt <<- NULL
    }
    get <- function() x 
    ##should retrieve contents of x
    setinv <- function(inverse)mt <<- inverse
    getinv <- function()mt
    ##should retrieve inverse of matrix object if stored
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


##solves and saves for inverse of matrix object

cacheSolve <- function(x, ...) {
  mt <- x$getinv()
  if(!is.null(mt)) {
    message("getting cached data")
    return(mt)
  }
  data <- x$get()
  mt <- solve(data, ...)
  x$setinv(mt)
  mt
}

        ## Return a matrix that is the inverse of 'x'


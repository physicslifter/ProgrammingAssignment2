## Function for speeding up repeated inverse matrix calculations
## caches the result of the matrix inverse
## When finding the inverse, first check whether
## the value has already been cached

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL ## im = inverse matrix
  }
  get <- function() x
  set_inverse_matrix <- function(inverse_matrix) im <<- inverse_matrix
  get_inverse_matrix <- function() im
  my_list <- list(set = set, 
       get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
  my_list
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse_matrix()
  if (!is.null(inverse)) {
    message("Retrieving from Cache")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$set_inverse_matrix(inverse)
  inverse
}

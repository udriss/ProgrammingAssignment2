## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is function that return a list containing a function to :
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(new_matrix = matrix(1:4,ncol=2,nrow=2)) {
  inverse <- NULL
  
  set <- function(new_matrix) {
    message("The default matrix is erased and replaced by the given one.")
    the_matrix <<- new_matrix
    inverse <<- NULL
  }
  
  if(dim(new_matrix)[1] == 2) {
    if(new_matrix==matrix(1:4,ncol=2,nrow=2)){#Do nothing }
  }}
  else {# Set new matrix
    set(new_matrix)
  }
  
  if(!is.null(inverse)){
    message("Inverse is cached : ")
    inverse
  }
  else{
    message("No cached inverse for the matrix")
  }
  
  get <- function() the_matrix
  setinverse <- function(givin_inverse) inverse <<- givin_inverse
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then
# the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(the_matrix, ...) {
  
  inverse <- the_matrix$getinverse()
  
  # cacheSolve should retrieve the inverse from the cache.
  if(!is.null(inverse)){
    message("Here the caching inverse of the matrix :")
    return(inverse)
  }
  
  # Return a matrix that is the inverse of 'x'
  matrix_to_inverse<-the_matrix$get()
  print(matrix_to_inverse)
  if (det(matrix_to_inverse)!=0) {
    message("The inverse of the matrix must be calculated (no cache) ... ")
    inverse<-solve(matrix_to_inverse)
    the_matrix$setinverse(inverse)
    message("The inverse is :")
    inverse
  }
  else(message("This matrix is ot inversible, the determinant is zero"))
}

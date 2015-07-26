## Below Code is written by Kamaljeet Singh for programming assignment 2 (R programming at Coursera)
## makeCahceMatrix function takes a matrix as input argument and does following:
##  1. Sets variable inverse_matrix to NULL
##  2.set_matrix function Assigns the matrix vector (input) to the set_matrix variable in parent environment and also assigns
## inverse_matrix variable to NULL value in parent environment
## 3. get_matrix function simply returns the matrix when called
## 4. get_inverse_matrix retrieves inverse_matrix
## 5. set_inverse_matrix assigns the inverse_matrix in the parent environment
## 6. list() fucntion is used to store all the subfunctions of makeCahceMatrix function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set_matrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get_matrix <- function() x
  set_inverse_matrix <- function(solve) inverse_matrix <<- solve
  get_inverse_matrix <- function() inverse_matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse_matrix = set_inverse_matrix, get_inverse_matrix = get_inverse_matrix)
  
}


## The cacheSolve fucntion first checks if there is non "NULL" value already  assigned to 
##inverse_matrix variable. If yes then its return the value of "inverse_matrix". If it finds the 
## value to inverse_matrix to be "NULL" it computes inverse of the matrix by using "solve" function
## assigns to inverse_matrix and prints inverse_matrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$get_inverse_matrix()
  if(!is.null(inverse_matrix)) {
    message("getting inverse matrix from cache")
    return(inverse_matrix)
  }
  data <-x$get_matrix()
  inverse_matrix <- solve(data, ...)
  x$set_inverse_matrix(inverse_matrix)
  inverse_matrix
}


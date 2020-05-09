## Submission for Coursera Data Science Specialization Course#2 'R programming'
## Submitted by: Saurabh-Katariyar (github)

## This function defines a matrix and can be used to cache the inverse so that not everytime 
## calculation of inverse is needed

## Assumption : The given matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL   
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates inverse of matrix
## The function checks if the given matrix has inverse in cache 
## If not, the inverse is calculated and stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinverse()
  if(!is.null(Inv)){
    message('getting cache inverse')
    return (Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinverse(Inv)
  Inv
}

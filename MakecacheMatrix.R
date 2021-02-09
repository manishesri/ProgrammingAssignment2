
#Assigning the matrix
makecachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
}
#getting the matrix  
  get <- function(){x}
#Setting the Inverse Matrix  
  setInverse <- function(inverse){inv <<- inverse}
#Getting the Matrix
    getInverse <- function(){inv}
list(set = set, get = get,
     setInverse = setInverse, getInverse = getInverse
     )


cacheSolve <- function(x,....){
  #Return the Inverse Matrix of X
  inv <- x$getInverse()
  
  
  if(!is.null(inv)){
    message("getting cached Data")
    return(inv)
  }
  
  Matrix <- x$get()
  inv <- solve(Matrix) %*% Matrix
  x$setInverse(inv)
  inv
}
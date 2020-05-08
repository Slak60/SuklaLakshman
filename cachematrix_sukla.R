## This program assigns a matrix as an object and then pass this object to the 
## function makeCacheMatrix. 
## First create a special matrix object(myMatrix_object) that will cache its inverse
myMatrix_object <- makeCacheMatrix(m1)## myMatrix_object is assigned to makeCacheMatrix(m1)
m1 <- matrix(c(1/2,-1/4,-1,3/4),nrow=2, ncol=2)##m1 is a numeric default square matrix,
  ##while calling the function(makeCacheMatrix) with m1,it can be changed to any square matrix
makeCacheMatrix <- function(m1){
  m2 <- NULL ## m2 is an empty matrix object
  set <- function(n1){ 
  m1 <<- n1 ##
    m2 <<- NULL
  }
  get <- function() m1
    setinv <- function(solve)  m2 <<- solve
  getinv <- function() m2
  list(set = set, get = get,
         setinv= setinv, getinv= getinv)
}
## This function checks if the inverse of the matrix returned by the 
## makeCacheMatrix function has already been calculated
# then cacheSolve retrives the inverse of the cache

cacheSolve <- function(myMatrix_object) {
  n2 <- myMatrix_object$getinv() ## ass
  if(!is.null(n2)){
    message ("getting cached data")
    return(n2)
  }
## else $get function retrievs the matrix object
  data <- myMatrix_object$get()
  n2 <- solve(data) ## Return a matrix that is the inverse of 'm1' and assign to n2
  myMatrix_object$setinv(n2) ## $set function set the matrix n2 into the myMatrix_object
  n2 ## returns the inverse matrix n2 
}
  
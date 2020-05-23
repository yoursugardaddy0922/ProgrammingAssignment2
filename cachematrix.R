#Caching the inverse of the matrix
#makeCacheMatrix stores a special matrix object "x"
#makeCacheMatrix creates a special matrix object that is a list including the functions
##set the value of the matrix object
##get the value of the matrix object
##set the value of the inverse matrix
##get the value of the inverse matrix
makeCacheMatrix<-function(x=matrix()){
  inver<-NULL
  #setting the inverse to NULL temporarily for the future value.
  set<-function(y){
    x<<-y
    inver<<-NULL
    #define the function to set matrix object x to a new matrix object y
    #set the inverse matrix "Inver" to NULL
  }
  get<-function() x#return x
  setInverse<-function(Inverse) inver<<-Inverse
  getInverse<-function() inver
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  #return all the vectors
}


cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("obtaining the cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setInverse(inver)
  inver
}

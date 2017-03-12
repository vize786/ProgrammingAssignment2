## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that is able to cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  setinverse<-function(inv)inverse<<-inverse
  getinverse<-function()inverse
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

 ## cacheSolve: This function evaluates an invisible square matrix by going through the values within that matrix

 cacheSolve<-function(x,...){
   inverse<-x$getinverse()
   if(!is.null(inverse)){
     return(inverse)
   }
   ## solve (x): Computing the inverse of a square matrix can be done with the solve function.
   data<-x$get()
   inverse<-solve(data,...)
   x$setinverse(inverse)
   inverse ##return
 }

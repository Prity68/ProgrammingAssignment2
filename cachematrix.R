## Below functions get a matrix and compute its reverse.In case the reverse was 
## already calculated it gets the inverse from cache

## defines the functions for gettimg and setting Matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function(){x}
  setinverse<-function(inverse){
    i<<-inverse
  }
  getinverse<-function(){
    i
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Function returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  else{
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
  }
}

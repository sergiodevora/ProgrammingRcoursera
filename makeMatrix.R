##ProgrammingAssignment2


##Matrix

makeMatrix<-function (x=matrix()){
  invrs<-NULL
  set<-function(y){
    x <<- y
    invrs <<- NULL
  }
  get<-function() {x}
  setInvrs <-function(inverse)(invrs <<- inverse)
  getInvrs<-function() (invrs)
  list(set = set, get = get , setInverse = setInverse, getInverse = getInverse)
}

##catch
cachsolve<-function(x,...){
  minv <- x$getInvrs()
  if(!is.null(minv)){
    message("getting cached data")
    return(minv)
  }
  mat<-x$get()
  minv<-solve(mat,...)
  x$setInvrs(minv)
  minv
  
}

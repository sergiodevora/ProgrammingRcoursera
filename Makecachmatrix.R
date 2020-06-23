## This function creates a "matrix" object that can cache its inverse

makeMatrix<-function (x = matrix())
  {
  invrs<-NULL #set an empty inverse matrix
  
  set<-function(y) #assign the value of the matrix to x in the parent environment
    {

    x <<- y
    
    invrs <<- NULL #set an empty inverse matrix in the parent environment
  }
  
  get<-function() {x} #gets the value of the matrix
  
  setInvrs <-function(inverse)(invrs <<- inverse)#set the value of the inverse matrix in the parent environment
  
  getInvrs<-function() (invrs) #get the value of the inverse matrix
  
  list(set = set, get = get , setInverse = setInverse, getInverse = getInverse)# the following list of clouser functions is returned
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cachsolve<-function(x,...)
  {
  #gets the value of the inverse matrix
  minv <- x$getInvrs()
  # if inverse already exists the its cached value is returned
  if(!is.null(minv))
    {
    message("Catched Data")
    return(minv)
  }
  # otherwise , gets the value if matrix
  matr<-x$get()
  # and its inverse is calculated
  minv<-solve(mattr,...)
  # value of inverse is set/cached
  x$setInvrs(minv)
  #returns the inverse matrix
  minv
  }


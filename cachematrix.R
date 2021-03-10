# This algorithm gets a matrix and makes a list with the matrix and its inversion
#Then it can take a sqaure matrix and tries to find its inversion.

#makeCacheMatrix is a function that takes a matrix x and the output of it is a list of length 4 that has in
#each position the matrix itself, and some null matrices that are connected with functions.


makeCacheMatrix<-function(x=matrix()){
  inv<- NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getinverse<-function(){inv}
  list(set=set, get=get,setInverse=setInverse,getinverse=getinverse)
}

#CacheSolve has two scenarios. when a new square matrix is estimated, it calcuates the inversion of it. If it
# did it once then the second run of the same variable will just give the same message and a result,
#because it has alread been solved. 

cacheSolve<-function(x, ...){
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}
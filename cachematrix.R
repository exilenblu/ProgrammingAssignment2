## The functions below will allow you to cache the inverse of a matrix  rather then computing the it everytime.
## 
## The makeCacheMatrix creates a special "Matrix" object that can cache its inverse. The function creates a list of function that can; 
## set- set the dimension and content of the matrix to x and assign NULL to the variable i
## get- returns the matrix 
## setinverse- assigns the inverse of the matrix to the variable i
## getinverse- returns the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function will compute the inverse of the special "Matrix" turned by the makeCacheMatrix function.
## If the inverse of the matrix has already been computed, it will return " getting cached data" and return the value of i.
## The value of i is assigned using the getinverse function from the function makeCacheMatrix. If the inverse has not been 
## been computed than it will compute the inverse of the matrix and cache the result using the setinverse function.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
  
        ## Return a matrix that is the inverse of 'x'
}

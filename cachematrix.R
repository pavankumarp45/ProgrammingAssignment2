## when Matrix is provided as arguement to "makeCacheMatrix" function,
## it outputs list containing functions set,get,setinverse,getinverse
## The use of <<- operator is that it assigns new environment name 
## when each time you call this function and there are some changes in values of 
## variable "x". Like that "i" is also given same environment name as that of "x"
## 

## "makeCacheMatrix" creates matrix
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## When the returned value from "makeCacheMatrix" is provided as arguement
## "CacheSolve" function, it checks whether the inverse of matrix is "x" is
## calculated or not. If yes, it returns the cached result. If not it solves 
## the inverse of the matrix and the value is assigned to x$setinverse of that
## environment name. So that when same list is provided as arguement to
## "CacheSolve" function, it checks whether that arguement environment name is
## same as that of cached value variable environment name. If yes, it returns the 
## cached value.
##

## Calculates Inverse of Matrix if it is new, Otherwise returns Cache

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

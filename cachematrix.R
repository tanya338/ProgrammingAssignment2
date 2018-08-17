## The functions set the Cache Matrix, then attempt to solve it. If the functions find the answer in the Cache, they print a message, otherwise they solve the data and print it

## Setting the Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<-y
    i<<- NULL
  }
  get<- function()i
  setinverse<- function(solve)i<<- solve
  getinverse<- function(i)
    list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## Solving the Cache Matrix

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i<- solve(data,...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}



## Two functions below are used to create a special object that stores 
## a square matrix and cache's its inverse.

## 1. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## 1.1 - Determines if the input is a matrix
    if(is.matrix(x)==0){
    stop("input is not a matrix")
    }
    ## 1.2 - Determines if the matrix is square
      if(ncol(x)!=nrow(x)){
        stop("matrix is not a square matrix")
      }
    ## 1.3 - Start of matrix object creation and caching function
      inv <- NULL
      set<-function (y){
        if(is.matrix(y)==0){
          stop("input is not a matrix")
        }
          if(ncol(y)!=nrow(y)){
            stop("matrix is not a square matrix")
          }
          x <<- y
          inv <<- NULL
      }
      get<-function() x
      setInv<-function(i) inv <<- i
      getInv<-function() inv
      l=list(set=set,get=get,              ##assign element name to functions &
             setInv=setInv,getInv=getInv)  ##allows for $ extract operator 
}

## 2. This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), the cacheSolve function will retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
## 2.1 - Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse matrix")
    return(inv)
  }
  data<-x$get()
  i<-solve(data)
  x$setInv(i)
  i
}
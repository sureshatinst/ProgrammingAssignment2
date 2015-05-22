## Author: sureshatinst
## Class : R Programming

##The below programm is to solve the inverse of a matrix and cache it.

## If you input Matrix A, you get the inverse of it.
##Then again if you give Matrix A as input, It will get the inverse from Cache( makeCacheMatrix function )

##    INput    OUTPUT
##      A      inverse of A
##      A      Inverse of A from cache
##      B      Inverse of B 
##      A      Inverse of A  ( but not from cache, has to comput it)

## The program can only cache one  matrix at a time. And get flushed if you input a different matrix.
##Noe it will haeve in cache the inverse of second matix.


##Below function return the list containing to SET,GET,SETMEAN and GETMEAN.

makeCacheMatrix<- function(x = matrix()) {
  Inverse_matrix <- NULL
  
  ## Below are the function definition of set,get.setnverse,getinverse 
  set <- function(y) {
    x <<- y
    Inverse_matrix <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) Inverse_matrix <<- inverse
  getinverse <- function() Inverse_matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##An object is created

##Below function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  Inverse_matrix <- x$getinverse()
  
  ##if condition to find if we have the Inverse in cache.
  if(!is.null(Inverse_matrix)) {         
    message("getting cached data")
    return(Inverse_matrix)
  }
  
  ## If the condition is false, Need to compute inverse
  data <- x$get()
  Inverse_matrix <- solve(data, ...)
  
  ## Set and print the inverse  
  x$setinverse(Inverse_matrix)
  Inverse_matrix
}


#Verification:
#> c<-matrix(1:4,2,2)
#> m1<-makeCacheMatrix(c)
#> cacheSolve(m1)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> d<-cacheSolve(m1)
#getting cached data
#> c%*%d
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
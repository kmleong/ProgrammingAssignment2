## Assignment 2 for Coursera R-Programming (rpog015)
## Matrix inversion is usally costly comutation and there may be some benefit to 
## cahcing the inverse of a matrix rather than compute it repeatedly.  Following 
## two functions create a special object that stores a numeric matrix 
## and caches its inverse matrix. 


## First, create a matrix that containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## The following function returns the inverse of the matrix.  In the first stage,
## it checks weather the inverse has already been computed.  
## If the ans is yes, it will gets the result and skips the computation.  
## but, if the ans is no, it will computes the inverse and set the value 
## into the cache via setinverse function
 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}


## to test the above script, use the following code
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 10), c(2, 2)))
## > cacheSolve(m)

## output:
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.1

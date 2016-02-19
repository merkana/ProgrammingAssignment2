## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

## my makeCacheMatrix takes a myMatrix parameter which is type of Matrix object.  At first it sets the invrs 
## variable to null, it means it is initialized with null.
## the set function will then take a parameter called newMatrix and assign it to myMatrix object
## the get function is as always used to get my MyMatrix.  Finally the setInverse function takes the invrs parameter and
## set the inverse of myMatrix in the cache.


  makeCacheMatrix <- function(myMatrix= matrix()) {
        invrs <- NULL
         set <- function(newMatrix) {
             myMatrix <<- newMatrix
           invrs <<- NULL
         }
       get <- function() myMatrix
       setInverse <- function(inverse) invrs <<- inverse
         getInverse <- function() invrs
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
         
     }
  
  

## Write a short comment describing this function
## cacheSolve function takes the parameter mtrx and apply the getInverse method on it and it returns back 
## the already cached matrix.
  
  

cacheSolve <- function(mtrx, ...) {
  ## Return a matrix that is the inverse of 'mtrx'
  invrs <- mtrx$getInverse()
  if (!is.null(invrs)) {
    message("cached matrix")
    return(invrs)
  }
  newmat <- mtrx$get()
  invrs <- solve(newmat, ...)
  mtrx$setInverse(invrs)
  invrs
}


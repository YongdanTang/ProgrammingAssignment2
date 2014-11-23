## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #input x will be an matrix
  
     i <- NULL # i will be the inverse and it's reset to NULL 
    set <- function(y) { 
      x <<- y          #saves the input vector
      i <<- NULL       #resets the inverse to NULL
    }
    
    get <- function() { x }  #return the value of the original matrix
    
    setinverse <- function(inv) { i <<- inv} #called by cacheSolve during the first catchSolve 
                                             #access and it will store the value using superassignment
    
    getinverse <- function() { i } # return the catchSolve     
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #input x is an matrix created by makeCacheMatrix
        
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #access the matrix inverse
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get() #if x$getinverse returns NULL, read the data
  i <- solve(data, ...) #if i was NULL then inverse data
  x$setinverse(i) #store inversed matrix in i
  i #return the inversed matrix
  
}

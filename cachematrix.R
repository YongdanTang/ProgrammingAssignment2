## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # This function This function creates a matrix object that can cache its inverse. Input x will be an matrix
  
     i <- NULL # i will be the inverse and it's reset to NULL every time makeCacheMatrix is called
    
    set <- function(y) { # this function is used to reset the matrix
      x <<- y          # x saves the input vector using superassignment
      i <<- NULL       # resets the inverse to NULL
    }
    
    get <- function() { x }  # This funciton returns the value of the original matrix
    
    setinverse <- function(inv) { i <<- inv} # This is called by cacheSolve() during the first catchSolve 
                                             # access and it will store the matrix inverse using superassignment
    
    getinverse <- function() { i } # This function returns the cached matrix inverse on subsequent accesses     
    
    list(set = set, get = get,                # This is a list of internal funcitons so a calling function knows how to access these functions
         setinverse = setinverse,
         getinverse = getinverse)
  
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { # This function computes the inverse of the matrix if the inverse is not available 
                                 # input x is an matrix created by makeCacheMatrix
        
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # access the matrix inverse
  
  if(!is.null(i)) {              # if the inverse is available in cache, return the cached value               
    message("getting cached data")
    return(i)
  }
  
  data <- x$get() # if x$getinverse returns NULL, read the data
  i <- solve(data, ...) # if i was NULL then inverse the matrix using solve()
  x$setinverse(i) # store inversed matrix in i
  i               # return the inversed matrix
  
}

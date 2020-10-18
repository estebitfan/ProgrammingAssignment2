## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.  
makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          set <- function(y){
                    # <<- sets the variable outside the scope of the function
                    x <<- y
                    inverse <<- NULL
          }
          
          get <- function(){ x }
          setInverse <- function(solveMatrix){ inverse <<- solveMatrix }
          getInverse <- function() { inverse }
          
          #List of functions to set, get, setInverse and getInverse
          #From the special matrix
          list(set = set,
               get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# Checks if the mean was calculated before, if not: it calculates the mean.
cacheSolve <- function(x, ...) {
          
          # Return a matrix that is the inverse of 'x'
          # Inverse of a matrix
          inv <- x$getInverse()
          
          #If is not empty we get the inverse
          if(!is.null(inv)){
                    #Returns cached data
                    return(inv)
          }
          
          #Getting the matrix
          data <- x$get()
          
          #Solving the inverse
          inv <- solve(data)
          
          #Setting the inverse
          x$setInverse(inv)
          inv      
}



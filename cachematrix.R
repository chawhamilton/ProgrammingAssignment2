## Put comments here that give an overall description of what your
## functions do

## This will allow cachesolve to set, get,
## setinverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
  Inver <- NULL ## clears out if function used
  set <- function(y) { ##Allows you to set the matrix and clear inverse
    x <<- y 
    Inver <<- NULL
  }
  get <- function() x ##Allows you to pull the value of x
  setInverse <- function(solve) Inver <<- solve ##Allows you to set the value
  #for inverse
  getInverse <- function() Inver #Allows you to retrieve the value for inverse
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse) #Gets output to other functions
}


## This will run functions in makecache matrix only if the inverse
## is not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inver <- x$getInverse() #Checks the getinverse portion of the above function
  if(!is.null(Inver)) { #If it is not blank it just retrieves the function with a 
    ##message
      message("getting cached data and saving you time")
      return(Inver)    ## exits the function and returns Inv
  }
  data <- x$get() ##transfers matrix to a local variable
  Inver <- solve(data, ...) ##Inv is set to solve
  x$setInverse(Inver) ##Inver is set to value in above function
  Inver ##Output variable
}

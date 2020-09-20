## Put comments here that give an overall description of what your
## functions do
#Function makeCasheMatrix defines setters and getters for input matrix x and its
#calculated inverse matrix. It returns a special object which is a named list 
#of mentioned setters and getters functions.
#Function cacheSolve computes the inverse of the matrix stored in the environment  
#of the function makeCacheMatrix and stores its value in the environment of the 
#makeCacheMatrix

## Write a short comment describing this function
#Function returns a named list of following functions:
#1.Set function assigns the input argument to the object x and the NULL value
#to the object i in the parent environment. This clears any value of i that had 
#been cached by a prior execution of cacheSolve(). Call of function makeCacheMatrix
#also assigns the NULL value to the object i.
#2.Get function returns input object x from the parent environment
#3.Setinverse assigns calculated inverse to the object i in the parent environment 
#4.Getinverse returns object i from the parent environment
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#Function has to take as input parameter object returned by the makeCacheMatrix. 
#First it checks if the inverse has already been calculated by calling function
#getinverse(). Notice that calls of functions makeCacheMatrix and makeCacheMatrix$set 
#will assign value NULL to the cashed inverse, i.e. if input matrix was changed, 
#calculated inverse will be set to NULL
#If inverse matrix is stored in the environment of the function makeCacheMatrix, 
#message "getting cached data" will be displayed and function will return stored cached 
#inverse. Otherwise, function will retrieve matrix x stored in environment of the function 
#makeCacheMatrix, calculate its inverse and it will store it back in the environment 
#of the function makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}




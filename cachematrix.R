## Put comments here that give an overall description of what your
## functions do
##
## Creating the functions was fairly simple because you can just use the
## template of the example (which is what I did). Understanding what you are
## doing is more complicated.
## Many thanks to Thiago Balbo, his comments really helped me understand 
## the assignment. Check it out yourself: https://class.coursera.org/rprog-007/forum/thread?thread_id=707
## We are basically creating functions that create/call other functions
## For example, if M is a random invertible matrix,
## M and mat<-makeCacheMatrix(M) live in the Global environment.
## makeCacheMatrix creates 4 functions which are dependant on mat.
## cacheSolve(mat) calculates the inverse by calling the functions that were created at the
## makeCacheMatrix environment. That is possible due to R scoping rules.
## Then, if B is any other invertible matrix, then mat$set(B) sets the a new matrix and the
## inverse is calculated by calling cacheSolve(mat)

## This function creates 4 functions that are dependant on the given argument.
## It also works as a 'middle environment' between the global and the cachesolve environment.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function returns the inverse of a matrix, provided that it has been first
## introduced with the makeCacheMatrix function.
## If the inverse had been calculated beforfe, it is stored and thus is not calculated again
## The inverse is stored as m, a 'free' variable from the cacheSolve perspective, but defined in the
## makeCacheMatrix level.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

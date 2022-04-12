## Creates an object that stores and makes accessible an
## input matrix and its inverse for repeated usage


## makeCacheMatrix creates an object that includes 
## input matrix
## default inverse to null until it is recalled
## list of functions to store and retrieve the matrix and its inverse

makeCacheMatrix <- function(mat = matrix()) {
  inver <- NULL
  set <- function(y) {
    mat <<- y
    inver <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse_mat) inver <<- inverse_mat
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## verify whether the inverse exists
## if not calculate the inverse and set to memory for future recalls

cacheSolve <- function(mat, ...) {
  inver <- mat$getinverse() #Get cached inverse
  #if cached inverse is not null then retrieve value
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- mat$get() # get input matrix as 'data'
  inver <- solve(data, ...) #solve inverse of input matrix
  mat$setinverse(inver) #cache calculated inverse
  inver
}
        ## Return a matrix that is the inverse of 'x'

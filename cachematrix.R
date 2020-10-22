## Assignment Caching the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) { #first we create the "special" matrix
  mtx<-NULL
  set<-function(y){
    x<<-y
    mtx<<-NULL 
  }
  get<-function()x
  setinv<-function() mtx<<-solve(x) # then we get the inverse matrix
  getinv<-function() mtx
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

makeCacheMatrix()


m<-makeCacheMatrix() #testing the function
m$set(matrix(1:4,2)) #we set the value of the matrix
m$get() #we get the values
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
m$setinv() #we set the inverse matrix
m$getinv() #we get the inverse value
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#it works!


##The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
#the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value
#of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    mtx <- x$getinv()
    if (!is.null(mtx)) {
      message("getting cached data")
      return(mtx)
    }
    data <- x$get()
    mtx <- solve(data, ...)
    x$setinv(mtx)
    mtx
  }

cacheSolve(m)
# > cacheSolve(m)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5  
#it works!











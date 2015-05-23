##Your assignment is to write a pair of functions that cache the inverse 
##of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that 
##can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #names the function and identifies the input as a matrix
  m <- NULL #sets m to null so to pass to cacheSolve to let it know whether there is cached data
  set <- function(y) {
    x <<- y #assigns the input matrix x to Y
    m <<- NULL #sets the m to null 
  }
  get <- function() x #gets the input matrix x to pass to set
  setinv <- function(inverse) m <<- inverse #sets m to be a function of inverse which is identified as the solve() command in cacheSolve
  getinv <- function() m #gets the inversed m matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv) #returns a list of four functions
}


##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {#names the function and identifies the input. In this example it's the list of four functions returned above
  m <- x$getinv() #grabs m
  if(!is.null(m)) { #checks to see if m is set to null, if it is it means the data is already cached
    message("getting cached data.")
    return(m) #returns the cached matrix
  }
  data <- x$get()#if there isn't any cached data gets the matrix and assigns it to data
  inverse <- solve(data) #solves data creating an inverse matrix and assigning it to inverse
  x$setinv(inverse) #sets inverse to the inverted matrix
  m #returns m
}

#This is the set of commands I used to test the above functions
#start with a 3x3 matrix called mx
#mx<-matrix(c(1,5,3,2,4,2,4,5,3),3,3) 
#run the inverse to see the reults#
#solve(mx)
#here's the resulting inverse matrix
#[,1]       [,2] [,3]
#[1,] -3.333333e-01 -0.3333333  1.0
#[2,] -3.700743e-16  1.5000000 -2.5
#[3,]  3.333333e-01 -0.6666667  1.0
#run makeCacheMatrix with matrix mx and store the results in mCM. 
#mCM<-makeCacheMatrix(mx)
#if you run mCM you can see the results are the 4 defined functions
#set, get, setinv. getinv
#run cacheSolve with mCM and store the results in cS
#cS<-cacheSolve(mCM)
#cS now contains the same inverse matrix produced by solve(cS)
# cS
#[,1]       [,2] [,3]
#[1,] -3.333333e-01 -0.3333333  1.0
#[2,] -3.700743e-16  1.5000000 -2.5
#[3,] 
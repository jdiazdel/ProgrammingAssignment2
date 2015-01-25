#These two functions are meant to be used with each other in order to calculate
#the inverse of a matrix and cache the result so that the user won't have
#to wait for the inverse calculation the next time around.  A description of each
#function's purpose is provided before the function's code below.

#An example for how to use these functions:
#> a <- stats::rnorm(16)
#> dim(a) <- c(4,4)
#> b <- makeCacheMatrix(a)
#> b$get()
#[,1]         [,2]        [,3]       [,4]
#[1,] -0.8629889 -0.963798317  0.01867657 -0.1855983
#[2,] -1.9914886 -0.231388473  1.88707853 -0.4348552
#[3,]  0.7966338 -0.003598208 -0.24386861  0.0168227
#[4,]  0.4490889 -0.795746332  0.13819688 -0.7411337
#> cacheSolve(b)
#[,1]       [,2]      [,3]       [,4]
#[1,]  0.0258626  0.2703089  2.026172 -0.1190873
#[2,] -1.3453306 -0.3644477 -2.645089  0.4907016
#[3,]  0.2077302  0.9387496  2.874840 -0.5375709
#[4,]  1.4988715  0.7301421  4.603819 -2.0485446
#> cacheSolve(b)
#getting cached matrix
#[,1]       [,2]      [,3]       [,4]
#[1,]  0.0258626  0.2703089  2.026172 -0.1190873
#[2,] -1.3453306 -0.3644477 -2.645089  0.4907016
#[3,]  0.2077302  0.9387496  2.874840 -0.5375709
#[4,]  1.4988715  0.7301421  4.603819 -2.0485446
#> b$set(matrix(rnorm(16),4,4))
#> b$get()
#[,1]       [,2]        [,3]       [,4]
#[1,] -0.5248061  0.5497394 -0.06763739 -1.8585056
#[2,]  0.3623986 -0.3566427  0.02059311 -0.5245133
#[3,] -0.1314205 -0.6135326 -1.32337859 -0.4998878
#[4,] -0.5206736  0.6141640 -0.72278429  0.6011388
#> cacheSolve(b)
#[,1]       [,2]        [,3]        [,4]
#[1,] -0.3975130  4.8256498 -1.05422258  2.10491221
#[2,]  0.1632860  2.7385755 -1.06349628  2.00994927
#[3,]  0.1078592 -1.5615799 -0.15363525 -1.15682578
#[4,] -0.3814426 -0.4957782 -0.01129501  0.04225015

#makeCacheMatrix() Vector:
#A constructed vector that is used to store a matrix and
#it's inverse.  Vector is used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  #Assign "m" as null. m is designated as variable that
  #holds inverse matrix. (Initial value = Null vector)
  mx <- NULL
  
  #Function used to alter original matrix 
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  
  #Used to get values in x for cacheSolve() in order to compute
  #inverse matrix (Note: Original Matrix)
  get <- function() x
  
  #Used to store inverse matrix (use of cache)
  setsolve <- function(solve) mx <<- solve
  
  #used to retrieve the value (inverse matrix) of m in the cacheSolve()
  #function
  getsolve <- function() mx
  
  #Assign functions created above to list vector
  #This list will be referenced by cacheSolve()
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#cacheSolve():
#Meant to be used with makeCacheMatrix.  Purpose is to calculate
#the inverse matrix and to store (cache) the result for any future
#calls of an unchanged matrix.
cacheSolve <- function(x, ...) {
  #Get cached inverse matrix
  #mx will be null if not previously cached
  mx <- x$getsolve()
  #This checks to see whether "mx" is not null.  If not null
  #then it pulls the cached inverse matrix ("m")
  if(!is.null(mx)) {
    message("getting cached matrix")
    #returns cached inverse matrix and exits function
    return(mx)
  }
  
  #No cached matrix means this part of the function is ran
  
  #Calls get from makeCacheMatrix and retrieves the matrix
  #passed into makeCacheMatrix function assigning it to data
  data <- x$get()
  
  #Calculate the inverse Matrix
  mx <- solve(data, ...)
  
  #stores the output inverse matrix (cache)
  x$setsolve(mx)
  
  #returns mx = inverse matrix
  mx
}
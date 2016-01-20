## The 2 functions makeCacheMatrix cacheSolve enable retrieval of cached
## matrix inverse, rather than computing it everytime reducing execution time


## makeCacheMatrix is a special matrix which is really a list containing
## functions to:
##    1.set the value of the matrix
##    2.get the value of the matrix
##    3.set the value of the matrix inverse
##    4.get the value of the matrix inverse


makeCacheMatrix <- function( matrixToSolve = matrix()) {
    
## initialize the matrix to NULL
  solvedMatrix <- NULL
    
## set(modifyMatrix) will modify the matrixToSolve
    set<- function(modifyMatrix) {
      matrixToSolve <<- modifyMatrix
      solvedMatrix <- NULL
    }
        
## get() will return the matrixToSolve    
    get<- function() matrixToSolve
    
## solveInverse will solve the inverese of matrixToSolve
    solveInverse <-function(matrixInverse) solvedMatrix <<- matrixInverse
    
## getInverse returns the inverse of matrixToSolve
    getInverse <- function() solvedMatrix
    
## return a list of pointers to the functions
    list(set=set, get=get, solveInverse=solveInverse, getInverse=getInverse) 
}


## cacheSolve calculates the inverese matrix of the special "vector" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the inverse  
## matrix in the cache via the solveInverse function.

cacheSolve <- function(matrixFuncList, ...) {
  
## check the value of the solvedMatrix        
    solvedMatrix <- matrixFuncList$getInverse()
    
## if the inverese is solved before, return the solvedMatrix
    if(!is.null(solvedMatrix)) {
      message("Getting cached data.")
      return (solvedMatrix)
    }

## if it is not solved, then solve the matrix and set the value
    data<-matrixFuncList$get() #get matrixToSolve
    solvedMatrix <- solve(data,...) #solve matrixToSolve
    matrixFuncList$solveInverse(solvedMatrix) # set matrixToSolve
    solvedMatrix
}


## This first block of code creates a list of 4 functions that will be then used by second block of
## code to either set or get the inverted matrix in the cache. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                 ## initialize to NULL the matrix m
    set <- function(y) {      ## this function updates value of m in parent environment using superassign <<- operator
              x <<- y
              m <<- NULL      ## restore value to NULL
        }
    get <- function() x                           ## simple function that returns original input
    setInverse <- function(Inverse) m <<- Inverse ## enables to set matrix inverse in the parent env.
    getInverse <- function() m                    ## stores and subsequently enables cacheSolve() to recall the matrix inverse 
    
    list (set = set, get = get,                   ## store 4 functions created above as a list into makeCacheMatrix
    setInverse = setInverse, 
    getInverse = getInverse)
}


## this second block of code first checks if a matrix m is already stored into "getInverse". If this exists and is not NULL, 
## then it returns a message and the inverse matrix is not re-calculated. 
## In the other case, it gets the input (get) from makeCacheMatrix, calculates the inverse (using function "solve")
## and assign this to "setInverse".

cacheSolve <- function(x, ...) {
    m <- x$getInverse()    ## check if inverse of matrix is already stored in the function getInverse      
    if (!is.null(m)) {
      message ("getting cache data") ## if the the getInverse is not "null" a message appears and what is stored into getInverse is returned
      return(m)
      }
    matrix <- x$get()               ## if the getInverse is NULL then we get the original input matrix      
    m <- solve(matrix, ...)         ## calculate its Inverse using solve()
    x$setInverse (m)                ## set the restul of this calculation (ie, the inverse original matrix) into setInverse
    return(m)                       ## finally return the result of calculation
}

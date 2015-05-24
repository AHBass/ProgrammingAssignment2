## This function returns the inverse of a SQUARE matrix, with the added 
## feature that is caches the result so that, if called again later, it 
## will use the cached result rather than re-generating the result each time.
## USE: 1st:   a <- makeCacheMatrix(YourSquareMatrix)
##     THEN:   cacheSolve(a)  # solves for the inverse & returns result
##    LATER:   cacheSolve(a)  # recovers inverse from cache & returns result

## The first function (makeCacheMatrix) generates a list containing 
## set (set the value of the matrix), get (get the value of the matrix),
## setinverse (set the value of the inverse), and
## getinterse (get the value of the inverse)

makeCacheMatrix <- function(x = matrix()) {
     xinv <- NULL                            # creating xinv
     set <- function(y) {                    # 'set' function isn't used here
          x <<- y                            # but is included for consistency
          xinv <<- NULL                      # with the example given.
     }
     get <- function() {                     # defines 'get' to output 'x'
          x
     }
     setinverse <- function(inverse) {       # saves 'inverse' into 'xinv'
          xinv <<- inverse
     }
     getinverse <- function() {              # defines 'getinverse' to output
          xinv                               #   xinv
     }
     list (set = set, get = get,             # function returns list of these
           setinverse = setinverse, 
           getinverse = getinverse)
}

## This function (cacheSolve) checks if the inverse has already be generated.
## If so, it recovers the cached result; if not, it generates the inverse anew.

cacheSolve <- function(x, ...) {
     xinv <- x$getinverse()                 # looks at 'getinverse' of 'x' list
     if(!is.null(xinv)) {                   # if it is not null (has content)
          message ("getting cached data")
          return(xinv)                      # returns that content
     }
     data <- x$get()                        # stores the 'get' from 'x' list (x)
     xinv <- solve(data, ...)               # stores the inverse into 'xinv'
     x$setinverse(xinv)                     # caches the inverse 
     xinv        ## Return a matrix that is the inverse of 'x'
}

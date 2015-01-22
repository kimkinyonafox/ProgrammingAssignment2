## Matrix inversion is usually a costly computation and their 
## may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.  This assignment is to 
## write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can 
## cache its inverse.

## To test if this function works, assign a matrix to x in the
## console... x<- matrix (1:4, 2, 2)
## Assign the result of the function makeCacheMatrix(x) to a 
## variable... test <- makeCacheMatrix(x)
## Confirm there is nothing in test$getinverse()
## ...test$getinverse().....the result should be NULL
## Assign the inverse of the matrix to another variable
## ...sx <- solve(x)
## Assign the inverse to test$setinverse
## ...test$setinverse (sx)
## Check to see what is in test$getinverse
## the result should be the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

                m <- NULL
                set <- function (y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function (solve) m <<- solve
                getinverse <- function () m
                list (set= set, get = get,
                      setinverse = setinverse,
                      getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the 
## cache.

## These two functions are meant to work together.
## To test if this function works, make the test list created
## above the argument for cacheSolve... cacheSolve(test)
## The result should be the inverse of the matrix x.
## If you type cacheSolve(test) again, the string "Getting
## cached data" should print, and then the inverse of the matrix
## should be shown again.  This time the cached version.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## If inverse already exists, return cached data
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        ## Calculate a matrix that is the inverse of 'x'
        
        matrix_data <- x$get()
        m <- solve(matrix_data, ...)
        x$setinverse(m)
        m
}

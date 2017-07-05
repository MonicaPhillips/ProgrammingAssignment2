##This file contains two functions that cache a matrix.
##
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                      # set the value of the vector
                 x <<- y
                 m <<- NULL
        }
        get <- function() x                       #get the value of the matrix
        setInverse <- function(i) m <<- solve(x)  #set the inverse of the matrix
        getInverse <- function() m                #get the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){                         #determining if the matrix has 
                return(m)                        #already been calculated
        } 
        m <- solve(x$get())
        x$setInverse(m)                          #using the setInverse function 
        m                                        #to set the inverse matrix
}       

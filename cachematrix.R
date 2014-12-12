## This function avoids repeating costly matrix inversions by checking whether the matrix
## inverse has already been done, and if so loading the cached value of this calculation

## This function creates a special "matrix" object that can cache its inverse using get 
## and set from Object Orientated Programming.

makeCacheMatrix <- function(x = matrix()) { ## function input: the matrix 'x'
        m <- NULL                           ## inverse matrix, 'm', set to NULL every time 
                                            ## makeCacheMatrix is called        
        
        set <- function(y) {                ## this sets the values of matrix 'x' and
                                            ## inverse matrix 'm' which is NULL
                x <<- y
                m <<- NULL
        }
        
        get <- function() x                 ## gets the matrix 'x'
        setinverse <- function(solve) m <<- solve ## calculates inverse matrix 'm'  
        getinverse <- function() m          ## gets the inverse matrix 'm'
        list(set = set, get = get,          ## function output: list containing set, get,
             setinverse = setinverse,       ## set inverse and getinverse with 
             getinverse = getinverse)       ## set to NULL

}


## This function returns the inverse of 'x'. If the inverse of 'x' has already been
## calculated, it uses the cached value. If not it calculates the inverse of 'x'. 

cacheSolve <- function(x, ...) {           ## function input x: list from makeCacheMatrix
        
        m <- x$getinverse()                ## checks getinverse value in list x
        
        if(!is.null(m)) {                  ## if getinverse is not null uses cached value
                message("getting cached data")
                return(m)                 ## and the function stops 
        }
        data <- x$get()                   ## otherwise gets the matrix 'x'
        m <- solve(data, ...)             ## calculates the inverse 'm'   
        x$setinverse(m)                   ## and sets it to check against if function rerun
        m                                 ## function output: inverse 'm'
}


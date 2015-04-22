## These functions will compute the inverse of a matrix and then cache the result.
## If the result is already cached, the inverse won't need to be computed
## again.

## makeCacheMatrix will take an input of a matrix and keep record of it in
## memory. The variable c is set to NULL when the inverse hasn't been computed
## yet. 

makeCacheMatrix <- function(x = matrix()) {
     c <- NULL
     set <- function(y){
             x <<- y
             c <<- NULL
     }
     get <- function() x
     
     setinverse <- function(solve) c <<- solve
     getinverse <- function() c
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve looks up to see if the inverse has already been computed
##and returns the inverse. if not, it computes the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        c <- x$getinverse()
        if(!is.null(c)){   ##If c is'nt null, return the already computed inverse
                message("Retrieving cached data...")
                return(c)
        }
        
        mtrx <- x$get()
        c <- solve(mtrx, ...)
        x$setinverse(c)
        c
}

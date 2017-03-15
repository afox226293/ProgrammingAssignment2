## The following 2 functions work together to pass a matrix and return its inverse using
## solve().  As solve() can be a costly computation cacheSolve() computes the inverse of
## the matrix and stores it in memory within the parent directory allowing for it to be 
## rapidly called upon in subsequent runs of the function.  When passed a matrix as 
## argument x, makeCacheMatrix() returns a list of 4 functions which can then be passed
## to cacheSolve().

## makeCacheMatrix() accepts a matrix as argument x returns an object of type list 
## that can be passed to function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(a) {
                m <<- NULL
                x <<- a
        }
        get <- function() {
                x
        }
        setSolve <- function(solve) {
                m <<- solve
        }
        getSolve <- function() {
                m
        }
        list(set = set,
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}

## cacheSolve() accepts arguments of type list returned by makeCacheMatrix() and returns the 
## inverse of the matrix oringally passed to makeCacheMatrix(). It also stores the result
## in memory so if function cacheSolve() is called with the same argument again the result
## will be returned from the cache as opposed to being computed with solve().

cacheSolve <- function(x, ...){
        m <- x$getSolve()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        } else {
                mat <- x$get()
                m <- solve(mat, ...)
                x$setSolve(m)
                return(m)
        }
        
}



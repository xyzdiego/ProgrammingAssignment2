## Caching the Inverse of a Matrix
## Solve matrix is usually a cost computational function and there are some
## benefit to catching the solve of matrix repeatedly. 

## This function creates one type of "special matrix" saved in Cache

makeCacheMatrix <- function(x = matrix()) {
    # function for define the matrix and use a free parameters to include in cacheSolve
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x
        setInverse <- function(inverse){
            inv <<- inverse
        }
        #free function for call the inverse with cacheSolve
        getInverse <- function(){
            inv
        }
        #free function for call the inverse with cacheSolve
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #list function for calling that parameters with $
    }
}


## cacheSolve() is a function for calculate the inverse matrix to a "special matrix"
## created by makeCacheMatrix. If the inverse has calculated and the matrix is the 
## same, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
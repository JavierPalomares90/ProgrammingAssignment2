## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix works by storing the matrix and it's inverse as data fields of the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
    #initialize inverse matrix
    inv <- NULL
    # setter function for matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # getter function for matrix
    get <- function(){
        x
    }
    # setter function for inverse of matrix
    setinverse <- function(inverse){
        inv <<- inverse
    }
    # getter function for inverse of matrix
    getinverse <- function(){
        inv
    }
    # list of object methods
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## compute the invers of the matrix using the makeCacheMatrix
cacheSolve <- function(x, ...) {
    # grabe the inverse matrix
    inv <- x$getinverse()
    # if the inverse is null, return it
    if(!is.null(inv)) {
        message("fetching cached inverse matrix...")
        return(inv)
    }
    # else need to use solve() function to compute inverse, set it, and return it
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

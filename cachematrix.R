## The functions below ae written by analogy with the vector ones which has been provided as 
## an example.

## The "makeCacheMatrix" function creates a matrix and return a list of functions that allow 
## to operate with the matrix:
## - get the matrix;
## - set the matrix (i.e. change the existing matrix);
## - get the inversed matrix;
## - set the inversed matrix.
## The function returns a list of functions described above. These functions allow you to 
## interact with the matrix object.

## The "cacheSolve" functions calculates the inversed matrix (using "Solve" function) or 
## takes it from cache if it has already been calculated.

## The "makeCacheMatrix" function defines a matrix object and rules of interaction with it.

makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL
    set <- function (y){
        x <<- y
        iMatrix <<- NULL
    }
    
    get <- function() {x}
    setIMatrix <- function (invMatrix) {iMatrix <<- invMatrix}
    getIMatrix <- function() {iMatrix}
    
    list (set = set, get = get, setIMatrix = setIMatrix, getIMatrix = getIMatrix)
}

## The "cacheSolve" function returns inversed matrix, which was defined by the 
## "makeCacheMatrix" fucntion.

cacheSolve <- function(x, ...) {
    iMatrix <- x$getIMatrix()
    if (!is.null(iMatrix)){
        message("getting cached data")
        return(iMatrix)
    }
    tempMatrix <- x$get()
    iMatrix <- solve(tempMatrix,...)
    x$setIMatrix(iMatrix)
    iMatrix
}

## To check correctness of the functions I used the following code:

## sample <- matrix(rnorm(25),ncol = 5)
## x <- makeCacheMatrix(sample)

## cacheSolve(x)
## tmp <- cacheSolve(x)
## tmp

## x$set(matrix(rnorm(36),ncol = 6))

## cacheSolve(x)
## tmp <- cacheSolve(x)
## tmp
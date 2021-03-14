## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a matrix x as an argument; inv is assigned as NULL to delete any prior value attributed to it. ##
## setMx is described and it will be used to define a new matrix, so it will receive a new matrix and assign its value to the variable x. ##
## functions getMx (return the matrix), getMxinv (return matrix inverse) and setMxinv (to set a new matrix inverse) are also described. ##


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMx <- function(z) {
        x <<- z
        inv <<- NULL
    }
    getMx <- function() {x}
    getMxinv <- function() {inv}
    setMxinv <- function(solve) {inv <<- solve}
    list(setMx = setMx, 
         getMx = getMx, 
         setMx = setMx, 
         getMxinv = getMxinv)
    
}


## cacheSolve receives a matrix x as an argument and attempts to get its inverse. ##
## If this value is NULL, is calculated its inverse with the function solve and stores it in the variable inv. ##
## Inv is then printed to the console. ##

cacheSolve <- function(x, ...) {
    inv <- x$getMxinv()
    if(!is.null(inv)) { 
        message("retrieving matrix data")
        return(inv)
    }
    inv <- solve(x$getMx())
    print(inv)
    
    
}

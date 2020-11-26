############################
## Caching Matrix Inverse ##
############################

## IF YOU'RE READING THIS TO GRADE FOR COURSERA PAST NOVEMBER 2020 SOMEONE IS PLAGARIZING OFF MY WORK! FLAG IT!!!!!

## These functions are designed to first cache the inverse of a matrix, then find it.

## makeCacheMatrix produces a list of functions and an environment to run them in
## in this case set(), get(), setinverse(), getinverse()
## Comments at each line/after each line of function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## initializes m as Null - to be used later in the environment
    set <- function(y){
        x <<- y
        m <<- NULL
    } ## assigns the input to x in the parent environment (for use in later functions) and clears m
    
    get <- function() x ## return the x defined in the parent environment
    
    setinverse <- function(solve) m <<- solve 
    ## sets the function solve as the one called to calculate (set) the inverse
    ## note - this is just setting it up, the calculation is done in the next function
   
     getinverse <- function() m
    ## once cacheSolve is called, this allows the inverse to be stored and retrieved
    
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    ## put all these functions into a list, these can then be called later
}


## A function to retrieve the cached inverse of a matrix
## This REQUIRES an object that has gone through makeCacheMatrix, if you just want to solve a matrix
## Use the solve function, the idea is that you need it multiple times, and don't want to solve it
## more than once, so cacheSolve will retrieve it if it is already made, or calculate it.
## Comments at each line of function

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## get the value of m, NULL if never been called, otherwise is the inverse
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    } ## if m is NOT Null, return the already calculated inverse
    
    data <- x$get() ## if m is null, retrieve the matrix put into makeCacheMatrix
    
    m <- solve(data, ...) ## create the inverse of matrix x using solve
    
    x$setinverse(m) ## set inverse to the new value
   
     m ## print the solved inverse of the matrix
}


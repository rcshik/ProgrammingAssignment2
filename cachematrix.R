## Put comments here that give an overall description of what your
## functions do
## The following functions implement a caching scheme in R where a the results of a 
## potentially time consuming operation like a matrix inverse on a large invertible 
#  matrix can be done once and used multiple times, by storing (caching)  the value 
## and providing helper functions to retrieve the value.  This is feasible as long as 
## the matrix itself has not changed in value.
   
## Write a short comment describing this function

## Given a matrix x the function makeCacheMatrix generates a a special list that 
## contains helper functions that operate on the matrix to 
## (a) set the value of  the matrix
## (b) get the value of the matrix
## (c) set the inverse of the matrix
## (d) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function
## The cacheSolve function takes a special vecttor x (generated 
## through the makecacheMatrix function) as an input parameter
## and uses the functions in the special vector to get  the 
## inverse of the matrix. If the inverse is not computed yet 
## it is calculated using the Solve function and cached to 
## be retrieved later if needed.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}

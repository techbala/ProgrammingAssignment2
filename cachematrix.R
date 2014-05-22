## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Symbol m holds the calcuated inverse of a given matrix
 		m <- NULL
## function set is used to refresh the matrix ( x ) and the inverse ( m )
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
## function get is used to retrieve the matrix        
        get <- function() x
        
## setinverse is used to set the inverse of a matrix into the symbol m        
        setinverse <- function(inverse) m <<- inverse
        
## getinverse is used to get the inverse matrix assigned to the symbol m by the setinverse function        
        getinverse <- function() m
        
## list will be the output when you create an object for makeCacheMatrix. 
## This list contains the alias names for the private fucntions defined within the makeCacheMatrix. So this alias will be used to invoke the functions by using the object of makeCacheMatrix.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve function is accepts the object of makeCacheMatrix and invoke the getInverse method. Incase already the inverse has been calculated and cached in the object then retrieve the cached value.
## if thre is no inverse has been already calculated then get the matrix by using the get method and calculate the inverse by using the solve function and store the inverse value into the object by caling the setinverse function

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

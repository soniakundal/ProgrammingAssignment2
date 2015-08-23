
## The two functions enable the user to retrive a cached value of the
## inverse of a matrix in case it has been computed before. If not, 
## it computes the inverse of the matrix and stores the computed value 
## in the variable x_inverse and returns it from the cache the next time the 
## user asks to compute the inverse of the same matrix. 



## makeCacheMatrix takes an invertible matrix x as the input argument and 
## returns a list of three functions. 
## the first element of the list, get, returns the matrix assigned to x.
## the second element of the list, setinverse, assigns to x_inverse any value 
## that is passed to it. The third element, getinverse, returns the value of the
## variable x_inverse.


makeCacheMatrix <- function(x = matrix()) {
        x_inverse <- NULL
        get <- function() x
        setinverse <- function(b) x_inverse <<- b
        getinverse <- function() x_inverse
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes the output list of the function makeCacheMatrix as it's
## input argument and returns the cached computed inverse of an invertible matrix 
## in case it has been computed earlier. If not, it computes the inverse of the 
## matrix and caches it for later use by using the function setinverse in the 
## function makeCacheMatrix.

cacheSolve <- function(x) {
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data)
        x$setinverse(x_inverse)
        x_inverse
}

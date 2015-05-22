## These two functions solve for, cache and return inverses for an
## argument matrix x.

## This function takes a matrix x as argument.It creates a cache for the
## matrix inverse. And it creates four functions - set,get,setinverse,
## getinverse - which can carry out various operations on the matrix x or 
## on the matrix inverse cache inv.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL       #Create a null object to cache the matrix inverse.
        
        set <- function(y) {   # Create a function that replaces the matrix x
                x<<-y          # stored in the main function, and sets the
                inv<<-NULL     # object inv to null in the global environment 
        }                       
        
        get <- function() x   # Gets the matrix stored in the main function.
        
        setinverse <- function(inverse) inv<<-inverse     #Stores data in 
                                                          # the cache inv. 
        
        getinverse<- function() inv     # Gets the data stored in inv.
        
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)     # Stores the four functions.
}


## To return a cached value of the matrix inverse or to solve for and return
## the inverse of the matrix x.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()      # Gets the cached value previously 
                                   # stored under makeCacheMatrix.
        
        if(!is.null(inv)) {        # Checks if cached inv is Null. If not
                message("getting cached data")    # the function returns a 
                return(inv)        # message and the cached value of inv.
        }
        
        data <- x$get()         # Gets matrix x previously stored under
                                # makeCacheMatrix.
        inv <- solve(data,...)  # Solves for the inverse of matrix x
        x$setinverse(inv)       # Stores the inverse
        inv                     # Returns the value of inv
}

## These two functions cache the inverse of a matrix to avoid repeating computation

## makeCacheMatrix creates a special matrix called IM to cache the inverse of object x

makeCacheMatrix <- function(x = matrix()) {
    IM <- NULL
    
    set <- function (y) {
        x <<- y 
        IM <<- NULL
    }
    
    get <- function() x
    
    setInv <- function (inv) IM <<- inv 
    
    getInv <- function() IM 
    
    list (set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    IM <- x$getInv ()
    
    if (!is.null(IM)) {
        message ("getting cached inverse")
        return (IM)
        
    }
    
    data <- x$get()
    
    IM <- solve(data, ...)
    x$setInv(IM)
    
    IM 
}

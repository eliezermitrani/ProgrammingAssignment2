## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #The first function, makeVector creates a special "vector", which is really a list containing a function to
        #a. set the value of the vector
        #b. get the value of the vector
        #c. set the value of the mean
        #d. get the value of the mean
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inversa) inv <<- inversa
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Inverse of the original matrix input to makeCacheMatrix()
        inv = x$getinv()
        
        # if the inverse has already been calculated. Si ya se calculo la inversa
        
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("Warning: Using cached data")
                return(inv)
        }
        
        # but, if the inverse isn't calculated, calculates the inverse 
        
        new.inv = x$get()
        inv = solve(new.inv, ...)
        
        # We assign this new value of the inverse to the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}

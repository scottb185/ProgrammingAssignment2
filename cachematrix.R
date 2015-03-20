# scottb185
# This first part sets m (matrix inverse) and y to NULL, which provides a default
# if cacheSolve has not yet been used.  Setmatrix sets the value of the matrix,
# and x<<-y caches the inputted matrix so that cacheSolve can see if it has changed.  

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL 
        y <- NULL 
             
        setmatrix <- function(y) { 
                x <<- y 
                m <<- NULL
        }
                
        list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
        getinverse = getinverse)
        
}

# In this second part, cacheSolve compares the matrix to what was run previously
# and gets the inverse if it has already been calculated.  The first if statement
# checks to see if cacheSolve has # been run before and if the matrix has changed.
# If m is not null and the matrix hasn't changed it returns the inverse.
# Otherwise, it gets and caches the input matrix, # solves for the inverse, 
# caches the inverse, and returns the inverse.   

cacheSolve <- function(x=matrix(), ...) {
        
        m <- x$getinverse() 
        if(!is.null(m)){ 
        if(x$setmatrix() == x$getmatrix()) {return(m)}}
                else {
                y <- x$getmatrix() 
                x$setmatrix(y)                                 
                m <- solve(y, ...) 
                x$setinverse(m) 
                
                m} 
        }

                
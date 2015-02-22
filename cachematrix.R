
## This R script defines 2 functions - makeCacheMatrix and cacheSolve which will make use of the cache when determining the inverse of a matrix
##  



## - makeCacheMatrix
##   Accepts a matrix as input and 
##      uses the solve() function to calculate the inverse,
##      saving the result in the cache

makeCacheMatrix <- function(x = numeric()) {                    #Define makeCacheMatrix function
        m <- NULL                                           
        set <- function(y) {                                    
                x <<- y                                         #Cache the value of y
                m <<- NULL                                      #Initialize with NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve                 #Call solve function
        getsolve <- function() m
        list(set = set, get = get,                              #Define list with results 
             setsolve = setsolve,
             getsolve = getsolve)
}




## -cacheSolve
##  Accepts a matrix and determines
##  if the inverse is already stored in cache.
##  if yes
##        it displays a message to that effect and retrieves the inverse
##  else 
##        it calculates the inverse
cacheSolve <- function(x, ...) {                                #Define cacheSolve function
        m <- x$getsolve()                                    
        if(!is.null(m)) {                                       #Determine if value is already cached
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                                    #Obtain solution
        x$setsolve(m)
        m                                                        #Display result
}

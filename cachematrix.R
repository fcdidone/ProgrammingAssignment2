## Theses functions cache the value of an inverted matrix. So  when we
## need it again, it will be looked up first in the cache (Global Environment),
## This is useful for time-consuming computation because it saves time computing
## matrix that were already computated.

## This function creates a special "matrix" and save it in the cache, 
## also store the functions listed bellow in the Global Environment.

makeCacheMatrix <- function(x=matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list( set = set, get= get, 
              setsolve = setsolve, 
              getsolve = getsolve)
}


## This function gets the inverse of the matrix that was assigned in the function makeCacheMatrix.
## It first checks to see if the inverse of the matrix has been calculated and if it's calculated 
## it skips the computation and get the inverted matrix in the cache,
## if it's not calculated, it compute and set the matrix in the cache.

cacheSolve <- function(x,...){
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
##  if the matrix is cached, Return a matrix that is the inverse of 'x'
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
}
        
## Note: English is not my first language

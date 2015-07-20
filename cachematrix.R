## The functions written below will return the inverse of any given matrix.
## The important thing to notice is that they will reduce the time needed to cal-
## culate it, because it wont recalculate the inverse as it will be stored
##in the cache, so, if the calculation need to be redone, then it will just get 
#the result sotred.



#For this function we need to enter the data in the form matrix(c(data),nrow,ncol)
#so we have a matrix. Then it stores the data in the Cache.
makeCacheMatrix <- function(x = matrix()) {
                     
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setSolve <- function(Solve) m <<- Solve
                getSolve<- function() m
                list(set = set, get = get,
                     setSolve = setSolve,
                     getSolve = getSolve)

}


## This function tests if the inverse of the matrix has been calculated, if true 
#then the function recall the result from the cache. If false it calculates it
#and then stores the result into the cache.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m  #print the inverse of the matrix
}



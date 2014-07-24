## This R file contains 2 functions makeCacheMatrix(x) and cacheSolve(x). 
## It improves matrix Inverse functionality by caching the Inverse matrix.
## It uses Lazy Initialization for matrix Inverse. i.e. Inverse martix is not 
## cached until it is accessed first

## Caches matrix and initializes Inverse matrix to NULL
## Defaults to empty numeric matrix if no argument is passed

makeCacheMatrix <- function(x = matrix()) {

        inverse<-NULL
        
        set<- function(y) {
                x<<- y
                inverse<<- NULL
        }
        
        get<-function() x
        
        setInverse<-function(i){
                inverse<<-i
        }
        getInverse<-function() {
                inverse
        }
        
        list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function accepts List object returned by makeCacheMatrix(x) and returns 
## the cached value of Inverse Matrix
## If Inverse matrix is not cached then it computes Invere Matrix and caches
## it in List function argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()

        if(!is.null(i)) {
            message("Returing cached value")
            i
        }
                
        i <-solve(x$get(),...)
        x$setInverse(i)
        i
}

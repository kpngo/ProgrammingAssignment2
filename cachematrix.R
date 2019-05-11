## The two functions together takes a matrix as an input and outputs
## an inversed matrix of the input matrix. If the inverse has already been
##  calculated, then the cachesolve should retrieve the inverse from the cache
## instead of recalculate the inverse matrix again to save computation time.   

## The makeCacheMatrix function creat a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
                    x   <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) inv <<- solve
            getinverse <- function() inv
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix above. If the inverse has already been
#  calculated, then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
            message("getting cached matrix")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinvers(inv)
        inv
}

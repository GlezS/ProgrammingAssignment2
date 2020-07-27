## With this function you create the matrix and you can catching the inverse.
## in makeCacheMatrix I use setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # here we start with the inverse NULL
        set<- function(y){
                x <<- y
                inv <<- NULL 
        }
        get <- function(){x}## with get, we will get matrix x
        setInverse <- function(inverse){inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set , get = get, setInverse= setInverse, getInverse = getInverse)
}

## Here is for for compute the inverse.( return the matrix inverse)

cacheSolve <- function(x,...){
        inv<- x$getInverse()
        if(!is.null(inv)){ ## here we will check whether inverse is NULL
                message("getting cached data")
                return(inv)## for returne inverse value
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}

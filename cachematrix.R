## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


#To run:
#> x = matrix(1:4, 2, 2)
#> x
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> solve(x)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> m<-makeCacheMatrix(x)
#> cacheSolve(m)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m)
#getting cached data.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
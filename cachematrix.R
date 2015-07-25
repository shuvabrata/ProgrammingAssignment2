#function that returns a vector of functions that will operate on the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    #Create the set function to store the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #function to get the matrix 
    get <- function() x
    
    #function to store the inverse into a variable
    setinverse <- function(inverse) inv <<- inverse

    #function to get the inverse (not calculate)
    getinverse <- function() inv
    
    #return the list of functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#This function calculates the inverse of the matrix using the special vector of functions.
#It will look into the cache and if it does not exist in the cache, then it will
#calculate the inverse and store it in the cache
#See steps below to execute the functions as a sample.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    #Check if we have a cached value
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    #We dont have a cached value. Calculate it.
    data <- x$get()
    inv <- solve(data)
    
    #Store in the cache
    x$setinverse(inv)
    
    #return the inverse
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
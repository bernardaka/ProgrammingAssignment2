## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    inv <-NULL
    ##set the value of the matrix
    ##get the value of the matrix
    ##set the value of the inverse
    ##get the value of the inverse
    set <- function(y){
        x <<- y
        inv <<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    ##if the inverse has already been calculated 
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ##otherwise, calculate the inverse
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}

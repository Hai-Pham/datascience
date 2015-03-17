## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #inversed matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("the inverse of same matrix is already cached")
                message("result is")
                return(i)
        }
        
        data <- x$get() #no cache, load the matrix to compute
        i <- solve(data,...) #do the inversion by solve()
        x$setinverse(i) #assign i to x
        i #return the inversed matrix
}

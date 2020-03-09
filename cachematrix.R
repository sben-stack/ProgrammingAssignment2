## Create a special matrix and put into cache
# create the inverse of the matrix, unless already in cash 

makeCacheMatrix <- function(x = matrix()) {
# set inverse to NULL to start
        inv <- NULL
# set the value of the matrix 
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
# get the value of the matrix
        get <- function() x

# set the inverse value of the matrix
        setinverse <- function(inverse) inv <<- inverse
# get the inverse value of the matrix
        getinverse <- function() inv
# create the list with all the four functions as result to the function 
        list (set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
# read the inverse function from the 
        inv <- x$getinverse()
# if the inverse function is already defined retrieve the already cached data
# and return it    
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}

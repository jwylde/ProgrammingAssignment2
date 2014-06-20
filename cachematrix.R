## Put comments here that give an overall description of what your
## functions do

# The first function, makeCacheMatrix  creates a special "vector", 
# which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- matrix()
        set <- function(y = matrix()) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse = matrix()) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The following function calculates the inverse of the matrix created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
#via the setinverse function.

# Important. fFor this function, all matrices are assumed invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

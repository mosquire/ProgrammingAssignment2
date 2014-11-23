# This function creates a list of functions and stores the given matrix in a variable "x"
# "i" will hold any calculated inverses, but it starts at "NULL".
# "set" passes a new matrix to the list and resets "i".
# "get" returns the stored matrix
# "setinverse" stores the given value in "i". This is the cached inverse.
# "getinverse" returns "i"
# Once the functions are defined, it puts them in a list and names them.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# This function takes a list created by "makeCacheMatrix" and either returns the cached
# inverse or calculates the inverse and caches it.
# First it pulls "i" from the list and tests if it has a value. If it does, it returns
# the stored value, with a message, because that is the cached inverse.
# If "i" is NULL, then it pulls the matrix from the list, calculates the inverse, stores
# the inverse in the list and prints the inverse.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
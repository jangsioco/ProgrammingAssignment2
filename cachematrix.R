## makeCacheMatrix, receives the matrix to be inverted, and contains 
##      functions that store and retrieve the source and target (inverted) matrices
## cacheSolve, makes use of the sub-functions contained in makeCacheMatrix
##      to determine if the inverted matrix can be retrieved from cache
##      or if it should be computed via the "solve" function

## makeCacheMatrix, accepts matrix as an input, and serves as the container
##      for the functions - set, get, setinverse, getinverse. the subfunctions
##      facilitate the process on how the source matrix is assigned (set), and retrieved (get)
##      and how the target (inverted) matrix is assigned (setinverse), and retrieved (getinverse)
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseval) i <<- inverseval
    getinverse <- function() i
    list(set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve, utilises the makeCacheMatrix function to determine if the 
##      target (inversed) matrix to be returned is already computed or not, by
##      checking if the global variable - i,is null.
##      if it's not in memory (i, is null), then it calls the "solve" function to compute
##      the inverse of the matrix, and assigns it to the global variable - i
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

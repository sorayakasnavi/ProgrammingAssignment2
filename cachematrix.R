
## makeCacheMatrix creates a special matrix that allows caching the inverted value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     ## reset value to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }                                               ## used to update the special matrix
        get <- function() x                             ## used to get the value in the special matrix
        setinv <- function(inverse) inv <<- inverse     ## used to save the inverter value in the special matrix
        getinv <- function() inv                        ## used to report back the cached inverted value
        ## Create the special matrix below
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve is a function that provides the inverted value of the matrix. It will get the cached value of the matrix has not changed, if not it will calculate the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }                                               ## Checks if the inverted value is cached, it can simply report it
        ## the section below will calculate the inverted value and cache it for next time
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)           
        inv
}


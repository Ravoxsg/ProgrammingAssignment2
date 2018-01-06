## For this assignment I follow the same procedure as what is done with the mean of a vector example/

## The following function creates a matrix as a list with 4 basic methods. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}        


## The following function takes a matrix as defined previously, and returns its inverse. It first checks if that inverse is already stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## testing these 2 functions

x <- replicate(10, rnorm(10))
y <- makeCacheMatrix(x)
i <- cacheSolve(y)
test <- i %*% x
print(test) ##it gives us the identity matrix -- youpee !!!

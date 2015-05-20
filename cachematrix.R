## The two functions takes a matrix as parameter and creates a 
## new matrix with the same values which can cache its inverse
## The following is the sample output
## > A <- matrix(c(2,4,5,6),2,2)
## > m <- makeCacheMatrix(A)
## > cacheSolve(m)
## [,1]   [,2]
## [1,] -0.75  0.625
## [2,]  0.50 -0.250

## makeCacheMatrix takes a matrix as input and creates a list of functions 
## can can get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invvalue) inv <<- invvalue
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function takes the special matrix as input and either 
## compute the inverse if it is called the first time or get a cached 
## inverse of the matrix if it is called previously
## the following is the results of a sample run
## > cacheSolve(m)
## [,1]   [,2]
## [1,] -0.75  0.625
## [2,]  0.50 -0.250
## > cacheSolve(m)
## getting cached data
## [,1]   [,2]
## [1,] -0.75  0.625
## [2,]  0.50 -0.250


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <-x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

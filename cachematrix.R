##The first function, makeCacheMatrix creates a special “matrix”, which contains the below ##functions:-
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of cache (inverse of matrix)
##4. get the value of cache (inverse of matrix)

makeCacheMatrix <- function(x = matrix()) {
m <- NULL

set <- function(y) {
x <<- y
m <<- NULL
}

get <- function() x
setcache <- function(inverse) m <<- inverse
getcache <- function() m
list(set = set, get = get, setcache = setcache, getcache = getcache)
}


##The function cacheSolve calculates the inverse of the special “matrix” created with the ##makeCacheMatrix function
##It first checks to see if the inverse has already been calculated
##If the inverse of the matrix is available in the cache, then it uses the cache to ##retrieve the value of the matrix inverse 
##which saves the cost of the inverse computation 

cacheSolve <- function(x, ...) {
        m <- x$getcache()
        if(!is.null(m)) {
                message("  ... getting from cached data")
                return(m)
        }
        initial_matrix <- x$get()
        inverse_matrix <- solve(initial_matrix, ...)
        x$setcache(inverse_matrix)
        inverse_matrix
}
## Test run 
##> a <- makeCacheMatrix()
##  initialize the matrix
##> a$set(matrix(c(1,2,3,4),2,2))
## display the values
##> a$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
## First time execute
##> cacheSolve(a)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## Second run for cacheSolve
##> cacheSolve(a)
##  ... getting from cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##
## Try the same functions using different values in the matrix
##> a$set(matrix(c(5,6,7,8),2,2))
## display the values
##> a$get()
##     [,1] [,2]
##[1,]    5    7
##[2,]    6    8
## first time execute with the new value
##> cacheSolve(a)
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5
## Second run for cacheSolve
##> cacheSolve(a)
##  ... getting from cached data
##     [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5




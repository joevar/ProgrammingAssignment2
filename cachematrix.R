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
##If the inverse of the matrix is available in the cache, then it uses the cache to ##retrieve the value of the matrix inverse which saves the cost of the inverse computation 

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


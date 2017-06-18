## Matrix inversion can be particularly computationally intensive
## depending on the size of the matrix. When possible, it is best
## to simply compute the matrix inversion once and then retrieve
## its cache rather than recompute each time the inverse is needed.
## These two functions compute, create, and retrieve the cache.

## This function is used to create a cached copy of the matrix
## inverse (setinv), as well as retrieve a cached copy (getinv)

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m_inv <<- inverse
        getinv <- function() m_inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## This function checks whether or not the inverse of matrix x
## is cached. If it is, it pulls the cached value. If not, it
## uses the R function "solve" to solve for the inverse which
## then becomes cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}

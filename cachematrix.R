## Put comments here that give an overall description of what your
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        m_inv <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() {
                x
        }
        setinv <- function(m_inv) {
                m <<- m_inv
                m
        }
        getinv <- function() {
                m
        }
        n <<- list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        n

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        m <- n$getinv()
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        m <- n$get()
        m_inv <- solve(m)
        m <- n$setinv(m_inv)
        m
}

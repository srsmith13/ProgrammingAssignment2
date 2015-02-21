## Takes a matrix as input, calculates the inverse and caches its value.
## If inverse has already been calculate, retrieves value from cache.
## If inverse has not already been calculated, computes, returns, and caches inverse

## MakeCacheMatrix creates a special matrix of functions that
## Set the value of matrix
## Get the galue of matrix
## Set the inverse of the matrix
## Get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## set value of m to NULL as default
    y <- NULL ## set value of y to NULL as default
    set <- function(y) { ##set value of matrix
        x <<- y ## caches input matrix
        m <<- NULL ## set value of m to NULL if cacheSolve used
    }
    get <- function() x
    setinv <- function(solve) m <<- solve ##compute inverse of matrix and cache value
    getinv <- function() m ##return matrix m
    list <- list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}

## cacheSolve calls the functions stored in the matrix created by makeCacheMatrix
## If inverse already calculated, retrieve inverse from cache
## If inverse not already calculated, compute, cache, and return inverse

cacheSolve <- function(x, ...) {
    m <- x$getinv() ##apply getinv to matrix m, i.e. if inverse already calculated
    if(!is.null(m)) { ##if m is not NULL i.e. if cacheSolve has been run before
        message("getting cached matrix") ##alert that inverse already computed, retreiving from cache
        return(m) ##return the inverse matrix
    }
    data <- x$get() ##if m is NULL, get input matrix
    m <- solve(data, ...) ##compute inverse
    x$setinv(m) ##run setinv function to cache inverse
    m ##return inverse of matrix
}

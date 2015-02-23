## The purpose of these functions is to improve the performance of matrix
## inversion by caching the result and re-using it, if available.

##  This function creates a special "matrix" object that can cache its inverse.
## It defines 4 functions to allow setting and retrieving the inverse of a
## matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL
    set <- function(y) {
        x <<- y
        inv.mat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv.mat <<- solve
    getinv <- function() inv.mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
## I am using solve() function to calculate the inverse
cacheSolve <- function(x, ...) {
    inv.mat <- x$getinv()
    if(!is.null(inv.mat)) {
        message("getting cached data")
        return(inv.mat)
    }
    data <- x$get()
    inv.mat <- solve(data, ...)
    x$setinv(inv.mat)
    inv.mat
}

## Test Run
#  --------
# > xm <- matrix(rnorm(16), c(4,4))
# > xm
# [,1]       [,2]       [,3]      [,4]
# [1,]  0.157395608  0.1475184  1.7523295  0.532542
# [2,]  1.563192584 -0.4439004  0.7705227 -1.683896
# [3,] -0.001578546 -0.3527607  0.6533845  0.600737
# [4,]  0.617466351 -1.6160525 -0.4569379 -1.108430
# > m <- makeCacheMatrix(xm)
# > m$get()
# [,1]       [,2]       [,3]      [,4]
# [1,]  0.157395608  0.1475184  1.7523295  0.532542
# [2,]  1.563192584 -0.4439004  0.7705227 -1.683896
# [3,] -0.001578546 -0.3527607  0.6533845  0.600737
# [4,]  0.617466351 -1.6160525 -0.4569379 -1.108430
# > cacheSolve(m)
# [,1]       [,2]       [,3]       [,4]
# [1,] -2.247527  1.3989711  3.4405005 -1.3404445
# [2,] -0.201244  0.2803896 -0.2519317 -0.6591869
# [3,]  1.235611 -0.2993687 -1.1225379  0.4400553
# [4,] -1.467976  0.4939294  2.7466401 -0.8692264
# > cacheSolve(m)
# getting cached data
# [,1]       [,2]       [,3]       [,4]
# [1,] -2.247527  1.3989711  3.4405005 -1.3404445
# [2,] -0.201244  0.2803896 -0.2519317 -0.6591869
# [3,]  1.235611 -0.2993687 -1.1225379  0.4400553
# [4,] -1.467976  0.4939294  2.7466401 -0.8692264
# >
## Test Run 2
# > xm2 <- matrix(rnorm(16), c(4,4))
# > m2 <- makeCacheMatrix(xm2)
# > cacheSolve(m2)
# [,1]       [,2]       [,3]       [,4]
# [1,] -1.515084  1.7729212 -1.2517250  2.3941260
# [2,] -1.035454  0.2025553  0.2100828  0.7895225
# [3,]  3.694819 -1.4558636  2.6004276 -3.7876747
# [4,] -1.110442  0.6547134 -0.7053454  1.6813668
# > cacheSolve(m2)
# getting cached data
# [,1]       [,2]       [,3]       [,4]
# [1,] -1.515084  1.7729212 -1.2517250  2.3941260
# [2,] -1.035454  0.2025553  0.2100828  0.7895225
# [3,]  3.694819 -1.4558636  2.6004276 -3.7876747
# [4,] -1.110442  0.6547134 -0.7053454  1.6813668
# >
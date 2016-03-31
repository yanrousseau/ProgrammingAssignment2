################################################################################
## Functions used to cache the inverse of a matrix.
## Author : Yannick Rousseau
## Date   : 31 March 2016
################################################################################

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Computes the inverse of the matrix created with 'makeCacheMatrix'.

cacheSolve <- function(x, msg = FALSE, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        if (msg == TRUE) message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

## Tests the functions 'makeCacheMatrix' and 'cacheSolve' as
## above while providing the time elapsed.
## To verify if caching works, use: testCMTime(len) where
## len >=10 and len <= 1000. Results (values in microseconds):
##  len     Time#1     Time#2  Time#1>>Time#2?
##    1      87668      18390  Yes
##   10     132571      22238  Yes
##   50     262148      14541  Yes
##  100    1020366      21383  Yes
##  500  123781527      22666  Yes
## 1000  765215035      22238  Yes
## Caching  improves time.

testCMTime <- function(len = 3, msg = FALSE) {
#   install.packages("microbenchmark")
#   library(microbenchmark)
    mat    <- matrix(rnorm(len**2),len,len)
    cmat   <- makeCacheMatrix(mat)
#   Calculation #1:
    mbm    <- microbenchmark(cacheSolve(cmat, msg), times = 1, unit = "us")
    matInv <- cmat$getInv()
    if (msg) {
        print("Initial matrix:")
        print(mat)
        print("Inverse matrix:")
        print(matInv)
    }
    print('Elapsed time #1 (us):')
    print(as.data.frame(mbm)[1,2])
#   Calculation #2:
    mbm    <- microbenchmark(cacheSolve(cmat, msg), times = 1, unit = "us")
    matInv <- cmat$getInv()
    if (msg) {
        print("Initial matrix:")
        print(mat)
        print("Inverse matrix:")
        print(matInv)
    }
    print('Elapsed time #2 (us):')
    print(as.data.frame(mbm)[1,2])
}
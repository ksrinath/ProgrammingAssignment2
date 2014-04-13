# makeCacheMatrix(matrix) -  A function that enables associating a cached value
# to the input matrix; the object returned by this function is referred to as
# 'cachingMatrix'

# cacheSolve(cachingMatrix) - Uses a cachingMatrix to cache the "inverse" of a
# matrix

# makeCacheMatrix() and cacheSolve() together enable caching of the "inverse" of
# a given matrix.
#############################################################################
# Details of makeCacheMatrix(): Creates a vector of functions to associate a
# cached value with a given matrix. The returned list contains a function to
# 1. set the input matrix
# 2. get the input matrix
# 3. set an associated (cached) value for the input matrix
# 4. get the associated (cached) value for the input matrix, or NULL if no
# cached value is present
makeCacheMatrix <- function(x = matrix()) {
    cachedValue <- NULL # the cached value
    
    # functions to set/get the input matrix
    set <- function(y) {
        x <<- y
        # clear cached value, as input matrix has changed
        cachedValue <<- NULL 
    }
    get <- function() x
    
    # functions to set/get the cached value
    setCachedValue <- function(cv) cachedValue <<- cv
    getCachedValue <- function() cachedValue
    
    # return a vector of these functions
    list(set = set, get = get,
         setCachedValue = setCachedValue,
         getCachedValue = getCachedValue)
}
############################################
# Details of cacheSolve():
# Input: A vector of functions as obtained by calling makeCacheMatrix()
# Output: Inverse of the matrix passed to makeCacheMatrix()
# Checks to see if the inverse has already been computed;
#  if yes, returns cached value
#  otherwise it calls solve() to compute the inverse, and populates the cache

cacheSolve <- function(x, ...) {
    # check cache first
    inv <- x$getCachedValue()
    # check if inverse was cached (i.e. if it is non-null)
    if(!is.null(inv)) {
        message("getting cached data")
        # return the cached value
        return(inv)
    }
    # not found in cache; need to compute inverse
    matrix <- x$get()   # the original input matrix
    inv <- solve(matrix, ...)   # compute inverse
    x$setCachedValue(inv)  # cache the inverse
    inv # return inverse
}

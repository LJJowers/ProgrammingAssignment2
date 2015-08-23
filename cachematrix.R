#cachematrix.R
# Leonard J. Jowers, 2015-08-23
## ============================================================================
## Notes:
## 1. The operator '=' is used instead of '<-' to make clearer where '<<-' is 
##    used & because '=' is the ubiquitous assignment operator.
## 2. '<<-' and '->>' are normally only used in functions, and cause a search 
##    to be made through parent environments for an existing definition of the 
##    variable being assigned. 
## 	  If such a variable is found and not locked, its value is redefined, 
##    otherwise assignment takes place in the global environment.
## 3. Avoided single character names. 
## 4. Used 4 space tabs, but had them converted to spaces (prefer 2 spaces).
## ============================================================================
## Function 'makeCacheMatrix' defines cache functions for the provided matrix
##  (set,get,setInverse,getInverse)
makeCacheMatrix = function(hiddenMatrix = matrix()) {
    # commment to make explicit that there is a hidden matrix
    hiddenInverse = NULL # on instantiation NULL the hidden inverse

    # define a function to set the internal matrix to be the new matrix,
    # and the inverse to be NULL
    set = function(newMatrix) {
        hiddenMatrix <<- newMatrix
        hiddenInverse <<- NULL
    } ## end of set

    # Return the value of the hidden matrix
    get = function() hiddenMatrix

    # Set inverse to be the provided value.
    setInverse = function(newInverse) hiddenInverse <<- newInverse

    # Return the stored value for inverse
    getInverse = function() hiddenInverse

    # Make the functions external
    list(set = set,               get = get, 
        setInverse = setInverse, getInverse = getInverse)
} # end of makeCacheMatrix

## Function 'cacheSolve' is a gateway to the cache function getInverse
cacheSolve = function(cachedMatrix, ...) {
    # Get whatever valuse is in the cache for the inverse.
    inverseMatrix = cachedMatrix$getInverse()
    if(!is.null(inverseMatrix)) { 
        message("Returning cached inverse matrix")
    } # end of leg NOT NULL
    else { # it was NULL so compute it.
        inverseMatrix = solve(cachedMatrix$get(), ...)
        cachedMatrix$setInverse(inverseMatrix) # and set it
    } # end of leg WAS NULL, now computed
    inverseMatrix
} # end of cacheSolve

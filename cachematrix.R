##Dylan Pudwill Assignnment#2: Lexical Scoping

## Both functions together are used in conjunction to 
## solve for the inverse of a matrix. Also, if the
## inverse has been computed already, the functions will
## retrieve cached value instead of recomputing the value

## 'makeCacheMatrix' will do the object oriented portion
## it will create accessors and mutators
##!!assuming matricies are all invertable, doing no error checking!!

makeCacheMatrix <- function(mat = matrix()) {
    
    ##every time a new matrix is made the cached value must be reset to NULL
    inv <- NULL
    
    ##can be used to change values in the matrix. Must remember to reset cache
    ## ...because matrix values are changing.
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    
    ## returns value of current matrix
    get <- function() {
        mat
    }
    
    ##after calculating the inverse, this function is used to cache the value
    setInv <- function(inverseReal) {
        inv <<- inverseReal
    }
    
    ##retrieve inverse value from cache
    getInv <- function() {
        inv
    }
    
    ##returns each function to the parent enviornemnt for late use by
    ## ...  'makeCacheMatrix' objects
    list(set = set, get = get, setInv = setInv, getInv = getInv)
    
}


## 'cacheSolve' will use the accessors and mutators from 
## 'makeCacheMatrix' to create the inverse and check for cache validity

cacheSolve <- function(mat, ...) {
    
    ##get the cached 'inv' value from 'makeCacheMatrix' function
    inv <- mat$getInv()
    
    ## if 'inv' doesn't equal 'NULL' => the inverse has already be solved for
    if(!is.null(inv)) {
        message("retrieving cached inverse matrix data...")
        return(inv)
    }
    
    ##case if 'inv' == 'NULL' => get matrix data, solve inverse, cache value
    ##matrixData <- mat$get()
    inv <- solve(mat$get()) ##error
    mat$setInv(inv)
    inv
}

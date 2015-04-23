## These functions allow us to create a special matrix and allow inverse of this 
## matrix to be calculated and cached if the same matrix inverse is computed again
## it will use the cached value and not actually compute the inverse again
## Function assumes that matrix is square and invertible, if not it may
## return value that is provided by solve function but may not be an inverse for the 
## matrix.
##
## The first function create a special Matrix which is a list
## containing functions to set the matrix value, get the matrix,
## compute the inverse of the matrix and get the inverse of matrix
## 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list (set = set,get = get, 
          setinv = setinv, 
          getinv = getinv)  
    
}


## This function when passed with handle of special matrix (list) created
## will compute or return cached value of the inverse of the matrix 
## Message cached indicates for testing that cached value was used

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        print("cached")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## this function is written for testing above functions to show 
## that when cacheSolve is called for mym1 (special matrix), first time
## inverse is computed and 2nd time it returns the cached value
## invertible matrix of 3x3 is used and check is made that 
## A %*% inv(A) is identity matrix and also computed and cached inverse returned
## are same. I had to use floor on matrix multiplication as there may be rounding 
## issues in solve while computing inverse.
## print statement to assert tests and final values of A(test matrix) %*% inv(A)
## is done to show result as identity matrix

mytest_cache <- function() {
    mym1 <- makeCacheMatrix((rbind(c(2,1,1),c(3,2,1),c(2,1,2))))
    print ("Compute inv first time for mym1")
    inv1<- cacheSolve(mym1)
    print ("Compute inv 2nd time for mym1")
    inv2<- cacheSolve(mym1)
    myident1 <- diag(nrow=3)
    if (identical(floor(mym1$get() %*% inv1),myident1)) print ("inv1 is correct using multiplication of matrix with inv to get identity")
    if (identical(inv1,inv2))print ("inv1 computed and inv2 from cache are identical")
    
    print(class((mym1)))
    print(class((inv1)))
    print(class((myident1)))
    mym1$get()
    inv1
    floor(mym1$get() %*% inv1)
}

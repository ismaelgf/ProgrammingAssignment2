## This R code returns the inverse of a matrix and store it into cache.
## If the inverse of a matrix has already been calculated, the code access
## to cache memory and obtain the inverse of that matrix.
## If the inverse of a given matrix have not been calculated, the code 
## calculates the inverse of the matrix and store it into cache memoery.

## Write a short comment describing this function
## This function 
## 1. assigns the data to a special matrix.


makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x<<-y
                inv<<-NULL
        }
        get<- function()x
        setinv<- function(inverse) inv<<-inverse
        getinv<- function()inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## This function:
## 1. Determines if the inverse has already been calculated.
## 2..If so, it skips the calculation and access the cache for the inverse.
## 3. If not, it calculates the inverse of the matrix and sets the value 
##    into the setinv() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
        if(!is.null(inv)){
                message("getting cache inverse")
                return(inv)
        }
        mat<- x$get()
        inv<- solve(mat)
        x$setinv(inv)
        message("new inverse matrix")
        inv
}

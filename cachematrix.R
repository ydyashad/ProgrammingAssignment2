## Put comments here that give an overall description of what your
## functions do

## The below function contains a list of 4 functions
## 1) set : sets the matrix.
## 2) get : gets the matrix.
## 3) setinv: sets the inverse of the matrix
## 4) getinv: gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) 
                {
        		x <<- y
                        inv <<- solve(x)
        	}
        get <- function() x
        setinv <- function(res) inv <<- res
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The below function returns the inverse of the matrix strored in the cache. 
## If the matrix is new and the inverse is not present in the cache, then it calculates the inverse of the matrix 
## and sets it in the cache and returns the inverse result.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return (inv)
}

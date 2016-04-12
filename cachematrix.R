## The variable makeCacheMatrix stores a function that creates a special "Matrix" wich is a list containing a function
## to:
## 1. Set the value of the Matrix.
## 2. Get the value of the Matrix.
## 3. Set the value of the inverse of the Matrix.
## 4. Get the value of the inverse of the Matrix.




makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set <- function(y){
                
                x<<- y
                i<<- NULL
        }
        
        get <- function()x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function()i
        list(
                set= set,
                get= get,
                setinverse=setinverse,
                getinverse=getinverse)

}


## cacheSolve variable stores a function that calculates the inverse of the special "Matrix" created in the 
## makeCaheMatrix function. It first ckecks if the inverse has already been calculated so it checks the data from
## cache and gets the inverse. If not, it calculates the inverse and sets it to the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <-solve(data,...)
        x$setinverse(i)
        i
}

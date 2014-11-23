
### creates special matrix object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
inv <- NULL #initialize variable to store inverse   
get <- function()   {x}  #get  original  matrixx
setinv <- function(solve){inv <<- solve} #solve inverse and cache it
getinv <- function()  {inv}  
list(get=get,setinv=setinv,getinv=getinv) #make these declared functions accessible
}


### computes the inverse of special matrix object returned by make cachematrix or  retrieves chache of already computed inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()   #get previously stored inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()   
        inv <- solve(data, ...)  #else calculate it.
        x$setinv(inv)
        inv
}

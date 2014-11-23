
### creates special matrix object that can cache its inverse,  assumes matrix is invertable   
makeCacheMatrix <- function(x = numeric()) {
inv <- NULL #initialize variable to store inverse  
get <- function()   {x}  #function to get  original  matrix
setinv <- function(solve){inv <<- solve} #function to set inverse result and cache it
getinv <- function()  {inv}  ##function to retrieve cached result
list(get=get,setinv=setinv,getinv=getinv) #make these declared functions accessible
}


### computes the inverse of special matrix object returned by make cachematrix or  retrieves chache of already computed inverse 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()   #get previously stored inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }     #else
        data <- x$get()  #get matrix 
        inv <- solve(data, ...) #calculate  inverse
        x$setinv(inv) #and  store cached result
        inv 
}

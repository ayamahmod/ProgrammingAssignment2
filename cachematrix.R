##The follwoing function makes cached matrix
##set the matrix, get the matrix, set its inverse, get its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #inverse of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x #return matrix
    setInv <- function(Inv) m <<- Inv #set inverse
    getInv <- function() m #return inverse
    list(set = set, get = get, #list of the functions in makeCacheMatrix
         setInv = setInv,
         getInv = getInv)
}


## The following function returns the matrix inverse, 
##if not calculated: calculate it, return it & cache it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv() #get inverse to check if calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get() #get matrix 
    m <- solve(data) #calculate its inverse
    x$setInv(m) #save inverse in cache
    m #return inverse
}

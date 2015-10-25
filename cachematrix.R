##The follwoing function makes cached matrix
##set the matrix, get the matrix, set its inverse, get its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y #x is the matrix
        m <<- NULL # m is the inverse of the matrix
    }
    get <- function() x #return the matrix
    setInv <- function(Inv) m <<- Inv #set inverse of the matrix 
    getInv <- function() m #return inverse of the matrix
    list(set = set, get = get,      #list of the function in makeCacheMatrix
         setInv = setInv,
         getInv = getInv)
}


## Calculate the inverse of the matrix and cache it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv() #check if inverse ic calculated or not
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get() #get the matrix
    m <- solve(data) #calculate the inverse
    x$setInv(m) #cache the inverse
    m #return it
}

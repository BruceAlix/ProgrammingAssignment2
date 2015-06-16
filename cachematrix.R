makeCacheMatrix <-function(x=matrix()){ #Creates a special matrice containing a list of functions
    inv <- NULL
    set<- function(y){  # set the  matrice to invert if it changed since last call
        x<<- y
        inv<<- NULL  # resets the inverse matrice as the input changed
    }
    get <- function()x   # gets the matrice to inverse
    setInv <- function(inverse) inv<<- inverse  # inverses the matrice 
    getInv <- function()inv # gets the inverse matrice
    list (set = set , get = get , getInv =getInv,setInv=setInv)
    
    
    
    
}
cacheSolve<-function(x, ...) {
    inv  <- x$getInv()
    if(!is.null(inv)) {  # checks in the cache if the invert matrice of x was already calculated
        message("getting cached data")
        return(inv)  # exits the function returning inv in cache
    }
    mat <- x$get()  # sets the matrice to inverse
    inv <- solve(mat,...) # inverts matrice input
    x$setInv(inv) # puts new value of invert matrice in the cache
    inv  #auto-print and returns the invert matrice
}

MakeCacheMatrix <- function(x = matrix()){
        Inverse <- NULL
        set<- function(y){
                x<<- y 
                Inverse<<- NULL                
        }      
        get<- function() x
        setInverse<- function(inv) Inverse<<- inv
        getInverse<- function() Inverse
        list(set = set,get = get,setInverse = setInverse,getInverse=getInverse)        
}
cacheSolve<- function(x,...){
        Inverse<- x$getInverse()
        if(!is.null(Inverse)){
                message("getting cached data")
                return(Inverse)
        }
        data<- x$get()
        Inverse<- solve(data)
        x$setInverse(Inverse)
}

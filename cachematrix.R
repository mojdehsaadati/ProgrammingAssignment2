MakeCacheMatrix <- function(x = matrix()){## in this function we generate a special matrix that has a list of function, set-get-setInverse-getInverese .
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
cacheSolve<- function(x,...){## in this function we assey wether the matrix has a inverse or not. If it does not we add its invevrse to its list
        Inverse<- x$getInverse()
        if(!is.null(Inverse)){
                message("getting cached data")
                return(Inverse)
        }
        data<- x$get()
        Inverse<- solve(data)
        x$setInverse(Inverse)
}

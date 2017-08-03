'the functions in this file will calculate the inverse
matrix of the object and cache the result to avoid the repetedly
computation on the identical object'

'x is the object to be processed, m is used to store
the processed object, the function will return a list element 
in which is a function defined in function makeCacheMatrix'

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    
    get<-function()x
    setinverse<-function(solve)m<<-solve
    getinverse<-function()m
    
    list(set = set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


'each time the function is called, it will check if the result
has been calculated. if calculated, the function will return 
the cached result, and if not, the function will calculate and cache
the result'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached inverse ")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
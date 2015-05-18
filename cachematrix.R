##------------------------------------------------------------------------------------------
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##------------------------------------------------------------------------------------------

## The function makeCacheMatrix creates a special "matrix", which is a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrixr
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    #initialise matrix property to contain inverse values 
    m<-NULL
    
    #method set
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    #method gets matrix x
    get<-function() {x}
    
    #method setInverse sets value for m
    setInverse<-function(solve) {m<<- solve}
    #method getInverse returns m
    getInverse<-function() m
    
    #list of functions returned from functions makeCacheMatrix
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}


##------------------------------------------------------------------------------------------
##cacheSolve: Function
##The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix() function. 
##First it checks to see if the inverse matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
##------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    #parameter x is type matrix with methods created by makeCacheMatrix
    #get property x.m
    m<-x$getInverse()
    #if m is in scope with cached value it is returned
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    #if m is null set temp varialbe matrix = x
    #set the inverse to m using scope
    matrix<-x$get()    
    m<-solve(matrix, ...)
    #set scope x property m = computed inverse
    x$setInverse(m)
    #return m
    m
}

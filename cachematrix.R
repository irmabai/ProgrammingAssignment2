## The two functions below are a group used for caching and solving inverse matrix of a given
## matrix, matrix could be input using the command like this: >myMatrix$set(matrix(data=c(0,1,1,0),nrow=2,ncol=2))


## makeCacheMatrix is used for creating matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve is used for calculating inverse matrix using the solve() function built in R

cacheSolve <- function(x, ...) {
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

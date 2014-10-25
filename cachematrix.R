##Programming assignment 2 consists of two functions that demonstrate the lexical scoping
##properties of R - a constructor function creating functions to be called by the second
##the duet creates a matrix per the parameter received (assuming it is inversible)
##and provides the functions to cypher, cache, and return the inverse of the matrix



##purpose of makeCacheMatrix is to build a matrix and 
##       4 callable functions to 
##1 set    - build the matrix
##2 get    - display the matrix if it exists
##3 setinv - figure and store the inverse of the matrix if it exists
##4 getinv - retrieve the solved matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL                       
        ##1 fnc for new matrix; also sets cached inv to NULL 
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        
        ##2 fnc for displaying matrix
        get<-function(){
                x
        }
        
        ##3 fnc for solving and storing in cache the inverse - 2b called from a function
        setinverse<-function(solve) inv<<- solve
        
        ##4 fnc for retrieving a cached value of the inverse - 2b called from a function
        getinverse<-function() inv
        
        ##create a vector of functions to allow usage 
        list(set=set,get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve function uses the variable x, which is a vector resulting from a 
##call to makeCacheMatrix function - this function uses x to  either retrieve 
##the cached value of the matrix inverse or call the function to solve for it 
## and returns inv (the inverse of the matrix)

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        ##if inv is in cache (not null), use inv value, otherwise solve 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        ##set the value x for this container to allow future detection
        x$setinverse(inv)
        ##return the value of inv (inverse of passed matrix)
        inv
}

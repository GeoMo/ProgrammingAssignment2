#Assignment2 - R Programming Course March 2015.

##Assignment: Caching the Inverse of a Matrix
## write a pair of functions that cache the inverse of a matrix
##function makeCacheMatrix - creates a special "matrix" object that can cache its inverse.
##function cacheSolve-computes the inverse of the special "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve should retrieve the inverse from the cache

##Comments
##makeCacheMatrix - Creates a list of functions 
##cacheSolve use these functions to get or set the inverted matrix in cache
##the matrix supplied must be a square matrix  to work,otherwise solve will return a error 

makeCacheMatrix <- function(x=matrix()) {
        # stores the cached value of a matrix
        # initialize to NULL
        m <- NULL
        # create the matrix in the working environment
        # Set 'x' for the function enviromnent to 'y'
        # Set 'm' for the 'makeCacheMatrix' environment to
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #Create a function 'get' to get the value of the matrix.
        #Needs the context of the cacheSolve to make sense.
        get <- function() x
        # invert the matrix and store in m. 
        setInverse <- function(inverse) m <<-inverse
        # get the inverted matrix from m. 
        #Needs the context of the cacheSolve to make sense  
        getInverse <- function() m
        # return the created functions to the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

##cacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
##if the inverse has already been calculated the cachesolve retrieves it 
#from the cache

cacheSolve <- function(x, ...) {
        #try to get the inverse of the matrix stored in cache 
        #(goes to x environment)
        m <- x$getInverse()
        # if inverse is not null return it from x
        
        if ( ! is.null(m)) {
                #the function prints the message
                message("getting cached data")
                #display matrix in console - the cached matrix
                return(m)
        }
        # else create the matrix in working environment 
        #and compute the inverse with solve function from R
        matrix <- x$get()
        m <- solve(matrix, ...)
        # assign the calculated inverted matrix in the 'x' environment 
        #using the 'setInverse' function
        x$setInverse(m)
        #display calculated matrix in console 
        return (m)
}

#####Tests
##create a matrix for testing functions - in working environment
my <- matrix(c(-1, -2, 1, 1), 2,2)
a <- makeCacheMatrix(my)
a$get()
##1st run is inverted matrix from working environment
inv <- cacheSolve(a)
inv

##2nd and subsequent runs - message
inv <- cacheSolve(a)
inv

###another test matrix
b <- makeCacheMatrix(matrix(1:4,2))
b$get()
b$getInverse()
cacheSolve(b)

### 
###test matrix * inverted matrix should yield the identity matrix and round it
d = b$getInverse()
id<-b$get() %*% d
round(id, 2)


## Functions that cache the inverse of a matrix

## makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
    		
	setthematrix <- function(y) 
	{
        	x <<- y
        	i <<- NULL
    	}

    	getmatrix <- function() x
    	setinversematrix <- function(inverse) i <<- inverse
    	getinversematrix <- function() i
	list(setthematrix = setthematrix,getmatrix = getmatrix,
	setinversematrix = setinversematrix,getinversematrix =getinversematrix)
}

##Calculate the inverse of the above created matrix  reusing cached result if available

cacheSolve <- function(x, ...) 
	{
        
		i <- x$getinversematrix()
    		if(!is.null(i)) {
        	message("cached data")
        	return(i)
    	}
    	
	m <- x$getmatrix()
    	i <- solve(m, ...)
    	x$setinversematrix(i)
    	i
## Returns a matrix that is the inverse of 'x'
}

##Usage:
## source('cachematrix.R')
## a1 <- makeCacheMatrix(matrix(c(4, 2, 2, 4), 2, 2))
##cacheSolve(a1)
##Result:
##           [,1]       [,2]
##[1,]  0.3333333 -0.1666667
##[2,] -0.1666667  0.3333333

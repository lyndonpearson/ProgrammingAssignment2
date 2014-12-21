##Lyndon Pearson
##R Programming
##Programming Assignment Two
##12/20/14

##################### General Description ################################
## As stated in Programming Assigment Two, the two functions below check
## to see if the inverse of a given matrix has been computed previously.
## If so, the value is returned from cache. If not, then the inverse
## is computed and stored.

## An example entry for the functions is between the quotation marks below:
## "x = matrix(c(1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8),nrow=4,ncol=4)"
## "y <- makeCacheMatrix(x)"
## "cacheSolve(y)"
## "cacheSolve(y)"
## The result displayed above can be checked with solve(x) for accuracy
##########################################################################


##################### makeCacheMatrix Description ########################
## In similar fashion as the example makeVector, this function
## first creates a "vector" that is a list containing functions that
## 1. get the matrix
## 2. set the inverse of the matrix in step one
## 3. get the value of the inverse of the matrix
##########################################################################
makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	get<-function() x
	setinverse<- function(inverse) inv<<-inverse
	getinverse<- function() inv
	list(get = get, setinverse = setinverse, getinverse = getinverse)

}

##################### cacheSolve Description #############################
## In similar fashion as the example cachemean, the below function
## calculates the inverse of the matrix of the special "vector" created with
## makeCacheMatrix function. If this inverse has already been calculated,
## the inverse is retrieved from cache and a message is displayed to console.
## Otherwise, the inverse is calculated and set in the cache via "setinverse"
##########################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <-x$getinverse()
	if(!is.null(inv)){
		message("getting cached inverse of matrix")
		return(inv)
	}
	data <-x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

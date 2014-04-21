## this R script contains two functions, 'makeCacheMatrix' and 'cacheSolve'. 
## these two functions allow caching the inverse of a matrix. 
## caching prevents having the same matrix inverted more than once.

## 'makeCacheMatrix' is a function that creates a cacheable matrix.
## 'x' is the matrix to be cached.
## return object is a list of four functions:
## (1) 'set', which overwrites the matrix with a new one;
## (2) 'get', which returns the values of the matrix;
## (3) 'setinv', which assigns its input to the inverse of the matrix; and
## (4) 'getinv', which returns the inverted matrix.

makeCacheMatrix <- function(x = matrix()){
	inv <- NULL # when the matrix is created, inverse is null
	set <- function(new){
		x <<- new
		inv <<- NULL
	} # alters the matrix; also removes cache
	get <- function(){
		x
	} # returns the matrix
	setinv <- function(newinv){
		inv <<- newinv
	} # assigns its input to the inverse of the matrix
	getinv <- function(){
		inv
	} # returns the inverted matrix
	message("cached matrix created.")
	invisible(list(set=set,get=get,setinv=setinv,getinv=getinv))
}

## 'cacheSolve' is a function that inverts a matrix. 
## if a cached solution is found, it is returned; 
## otherwise, an inverse is calculated, cached, and returned.
## 'x' is a list returned by 'makeCacheMatrix'.
## it is assumed that 'x' is invertible.
## return object is the inverse of the cached matrix.

cacheSolve <- function(x, ...){
	if(!is.null(x$getinv())){
		message("cached solution found. returning cached solution.")
		x$getinv()
	} # if cached inverse exists, return it
	else {
		message("cached solution not found. calculating new solution...")
		x$setinv(solve(x$get()))
		message("new inverse cached. returning cached solution.")
		x$getinv()
	} # otherwise, solve and cache new inverse
}

# for testing:
# thematrix = makeCacheMatrix()
# thematrix$get()
# thematrix$getinv()
# thematrix$set(matrix(c(2,1,1,4,1,5,9,2,7),3,3))
# thematrix$get()
# thematrix$getinv()
# cacheSolve(thematrix)
# thematrix$get()
# thematrix$getinv()
# cacheSolve(thematrix)
# round(thematrix$getinv()%*%thematrix$get(),6)

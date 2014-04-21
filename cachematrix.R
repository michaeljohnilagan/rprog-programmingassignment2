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

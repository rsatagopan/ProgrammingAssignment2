## cachematrix.R
## the following functions create the inverse of a matrix and
##  cache it for faster retrieval

## makeCacheMatrix gets/sets the matrix value and
## stores/retrieves the inverse

makeCacheMatrix <- function(x=matrix()) 
{
	mtrx <- x
	invrs <- NULL

	# --------------------------
	set <- function(m2) 	{
		mtrx <<- m2
		invrs <<- NULL
	}

	# --------------------------
	get <- function() 	{
		mtrx
	}

	# --------------------------
	setInverse <- function(m3)	{
		invrs <<- m3
	}

	# --------------------------
	getInverse <- function()	{
		invrs
	}

	# --------------------------
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

## cacheSolve retrieves cached inverse of matrix usine the 
## previous function. If not previously cached, calculates
## the inverse and caches it

cacheSolve <- function(x,...)
{
	m <- x$getInverse()
	
	if (is.null(m))
	{
		print("Not previously cached...")
		m <- solve(x$get())
		x$setInverse(m)
	}
	else
	{
		print("Getting cached value")
	}

	m
}

## Functions to create a special "matrix" object and retrieve the 
## inverse of the matrix. Supports square invertible matrices only.
## Optimized by caching the calculated inverse.
## - makeCacheMatrix: creates a special "matrix" object from a given matrix
## - cacheSolve: returns inverse of a matrix from an object created 
##   by makeCacheMatrix
##
## Example usage
## m <- matrix(nrow = 2, ncol = 2, c(1,1,2,4)) - creates 
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)

##
# Creates a special "matrix" object that can cache its inverse. 
#
# Parameters
# - x			Square invertible matrix. A proper parameter value 
#			can be constructed for example by calling function 
#			matrix(nrow = 2, ncol = 2, c(1,1,2,4))
#
# Returns special "matrix" object. Function cacheSolve can be used 
# to retrieve the inverse matrix.
##
makeCacheMatrix <- function(x = matrix()) {
	# initialize cached inverse to NULL
	inverse <- NULL

	# sets the matrix of which inverse will be cached and 
	# removes the current cached inverse
	set <- function(m) {
		x <<- m
		inverse <<- NULL
	}

	# returns the matrix
	get <- function() {
		x
	}

	# returns the inverse of the matrix. The inverse matrix is 
	# returned from the cache if possible. Otherwise it is 
 	# calculated and cached
	getInverse <- function(...) {
		# check if there is a cached inverse matrix
		if(is.null(inverse)) {
			# calculate inverse matrix and update cache
			inverse <<- solve(x, ...)
		}

		# return inverse matrix from cache
		inverse
	}

	# return special "matrix" object
	list(set=set, get=get, getInverse=getInverse)
}


##
# Computes the inverse of the special "matrix" returned by makeCacheMatrix. If
# the inverse has already been calculated (and the matrix has not changed), 
# retrieves the inverse from cache. 
#
# Parameters
# - x		Special "matrix" object constructed by makeCacheMatrix 
#		function
# - ...	Additional parameters supplied to solve-function used for
#		determining the inverse of a matrix (solve function is called 
#		only if inverse matrix has not yet been cached)
#
# Returns inverse of the matrix supplied to function makeCacheMatrix.
##
cacheSolve <- function(x, ...) {
	# retrieve inverse matrix, from cache if possible
	x$getInverse(...)
}

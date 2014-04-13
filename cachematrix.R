######################################################################################
## cachematrix.R
##
## This file contains two functions written for "Peer Assessment 2" (aka, "Programming 
## Assignment 2") of the Coursera R Programming class taught by Roger D. Peng, John 
## Hopkins Bloomberg School of Public Health.  Course start date: April 7, 2014.
## This file has been posted on GitHub as an assignment requirement.
##
## makeCacheMatrix and cacheSolve below are based on the provided functions makeVector
## and cachemean written by Roger D. Peng and his support team.
##
## Usage example (with a 3x3 matrix):
## 1. Create a 3x3 matrix for testing purposes
## > mymatrix <- matrix(c(1,3,2,0,7,1,9,9,1), 3, 3)
## 2. Create the "list object" needed as input to cacheSolve
## > mymatlist <- makeCacheMatrix(mymatrix)
## 3. Compute the inverse of mymatrix first checking if it has already been computed
## > mymatinverse <- cacheSolve(mymatlist)
## calculating inverse
## > mymatinverse
##             [,1]        [,2]        [,3]
## [1,]  0.01980198 -0.08910891  0.62376238
## [2,] -0.14851485  0.16831683 -0.17821782
## [3,]  0.10891089  0.00990099 -0.06930693
## 4. Re-invoke the cacheSolve function
## > mymatinverse <- cacheSolve(mymatlist)
## getting cached data
##
## April 13, 2014
######################################################################################

######################################################################################
## makeCacheMatrix - this function takes an NxN matrix as input and returns a list
##                   containing the following four functions:
##                   set, get, setsolve, and getsolve.
##                   This list object is in the expected format for input into the
##                   cacheSolve function. When cacheSolve is invoked, these four 
##                   functions are needed in order to cache the inverse of the NxN 
##                   matrix to avoid re-calculating it in a future function call 
##                   to cacheSolve.  
######################################################################################
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, 
	     get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}

######################################################################################
## cacheSolve - The function cacheSolve returns the inverse of an NxN matrix 'x'.
##              This function takes as an argument the list object returned from 
##              makeCacheMatrix. The function checks to see if m (the inverse) has 
##              already been calculated via the getsolve() function contained in the 
##              input list object and if TRUE then returns m (the inverse) that was 
##              cached earlier. If m has *not* already been calculated, then the 
##              NxN matrix whose inverse we wish to calculate is copied into the 
##              symbol "data" via the get() function stored in the list object created 
##              via makeCacheMatrix. The solve() function is then invoked (on "data")
##              to calculate the inverse. The inverse is cached via the setsolve()
##              function for future reference to avoid re-calculation. 
######################################################################################
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	message("calculating inverse")
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}

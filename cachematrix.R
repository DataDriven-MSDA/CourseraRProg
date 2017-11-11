# Author : Kumudini B
# Date : Nov 10, 2017
# Coursera RProgramming : Programming Assignment 2 (Week3)

## The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to

## set() : set the value of the matrix 
## get() : get the value of the matrix 
## setinverse() : set the value of the inverse of matrix
## getinverse() :  the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inversex) m <<- inversex
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     
     
        
}

## We assume that the matrix supplied is always invertible.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

## This function creates a special "matrix" object that can cache its inverse.
## It calculates the inverse of a matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache by getinverse() function and skips the computation. 
## Otherwise, it calculates the inverse of the data using solve(x) function to calculate inverse 
## and sets the value of the inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # If  no cahces data , compute the inverse and set it in 
     # cache by setinverse() function and then return the inverse of matrix computed.
     
     data <- x$get()
     m <- solve(data, ...) #getting the inverse of matrix using solve() function
     x$setinverse(m)
     m
}
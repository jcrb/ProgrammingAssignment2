## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#'@author JCB
#'@title makeCacheMatrix
#'@description store ans retrieve a matrix and his inverse
#'@param x an inversible matrix
#'@param inv.mat the inverse of x
#'@return a object of type makeCacheMatix (list)
#'who can store/retrive a matrix and his inverse
#'@method get return a matrix 
#'@method set store a matrix
#'@method setinv store the inverse matrix
#'@method getinv return the inverse of the matrix (or NULL)
#'@usage x <- makeCacheMatrix(mat)
#'@example mat <- matrix(c(-3,5,6,-1,2,2,1,-1,-1), nrow = 3)
#' x <- makeCacheMatrix(mat)
#' x$get()
#' 

makeCacheMatrix <- function(x = matrix()) {
        inv.mat <- NULL # inv.mat is local to makeCacheMatrix
        set <- function(y){
                x <<- y # superassignment of x in the scope of makeCacheMatrix
                inv.mat <<- NULL # and for inv.mat
        }
        get <- function() x # this method return the matrix
        getinv <- function() inv.mat # this method return the inverse matrix
        setinv <- function(im){ # this method store the inverse matrix
                inv.mat <<- im # superassignment of inverse in the scope of makeCacheMatrix
                message("Stored inverse matrix")
        }
        list(set=set, get=get, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function
#'@author JCB
#'@title makeCacheMatrix
#'@description handle a matrix object of type makeCacheMatrix and return his inverse.
#'If the inverse matrix exists, it is returned. Otherwise it is calculated and stored 
#'in the makeCacheMatrix object then returned.
#'@param x a makeCacheMatrix object
#'@param im the inverse matrix
#'@return a matrix that is the inverse of 'x'
#'@usage
#'@example  mat <- matrix(c(-3,5,6,-1,2,2,1,-1,-1), nrow = 3)
#' x <- makeCacheMatrix(mat)
#' cacheSolve(x)
#' cacheSolve(x)
#'
cacheSolve <- function(x, ...) {
        im <- x$getinv() # retrives the inverse matrix
        if(!is.null(im)){ # if exists the inverse matrix is returned
                message("Retrieves cached data")
                return(im)
        }
        data <- x$get() # else the matrix is retrived
        if(!is.null(data)){
                im <- round(solve(data),2) # and the inverse matrix is calculated
                x$setinv(im) # then stored
        }
        im # finally return the inverse matrix
}

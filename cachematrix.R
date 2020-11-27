## a<-makeCacheMatrix()         to log a into the functions in makeCacheMatrix
## a$set(matrix)                to set matrix in working environment
## cacheSolve(a)                to return inverse matrix

## makeCacheMatrix sets certain functions to be used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
## creates i and set it as NULL
        i <- NULL
## creates set as a function to store value: y in cache: x
## resets i to NULL in case other functions stored a value in i
        set <- function(y) {
            x<<-y
            i<<-NULL
        }
## creates get as a function to return the matrix x
  get<-function()x
## creates setinv as a function to store inverse of i as inv
  setinv<-function(inv)i<<-inv
## creates getinv as a function to return the stored inverse value
  getinv<-function()i
## set the functions to list again so that the $set $get $setinv and $getinv can work for new values
  list(set = set, get = get, setinv = setinv, getinv=getinv)
}


## cacheSolve returns inverse of matrix x
cacheSolve <- function(x, ...) {
## get inverse of matrix
        i<-x$getinv()
## return inverse of matrix, and show message if nothing is stored
        if(!is.null(i)){
            message("getting cached data")
            return(i)
         }
## creates data to store the value returned by x$get()
        data <- x$get()
## perform the inverse function solve for data and store as i
        i <- solve(data)
## store the inverted x as i in cache
        x$setinv(i)
## returns the inverted value
        i
}

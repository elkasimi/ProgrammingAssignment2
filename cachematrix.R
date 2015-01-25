## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#create a custumized matrix which stores the inverse
#and recomputes it only if the matrix is changed
makeCacheMatrix <- function(x = matrix()) {
			#init matrix inverse as null
            mi <- NULL
			
			#whenever we change the matrix the inverse must be recalculated
            set <- function(y) {
                    x <<- y
                    mi <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) mi <<- solve
            getinverse <- function() mi
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Write a short comment describing this function

#try to find a cached value of the inverse matrix otherwise
#it compute it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            mi <- x$getinverse()
            if(!is.null(mi)) {
                    message("getting cached matrix inverse")
                    return(mi)
            }
            data <- x$get()
            mi <- solve(data, ...)
            x$setinverse(mi)
            mi
}

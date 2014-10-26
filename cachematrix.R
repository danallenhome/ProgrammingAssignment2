##This function will cache the inverse of a matrix
##It will provide a way to get or set the inverse matrix

##This function will set the function using the solve function to ##calculate the inverse of the function and then a mechanism to ##get the stored matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCacheMatrix <- function(solve) m <<- solve
        getcacheMatrix <- function() m
        list(set = set, get = get,
             setCacheMatrix = setCacheMatrix ,
             getcacheMatrix = getcacheMatrix)
}

##This function will get the stored cache.  If the cache is null
##then it will calculate the inverse and then set it into the ##cache
cacheSolve <- function(x = matrix(), ...) {
	m<-x$getCacheMatrix()

	if(!is.null(m))
	{
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setCacheMatrix(m)
    
}

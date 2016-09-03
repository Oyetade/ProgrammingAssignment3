## Put comments here that give an overall description of what your 
## functions do 

## Write a short comment describing this function 

 
makeCacheMatrix <- function(x = matrix()) 
	{ 

		## variables to cache identity matrix and the input matrix
		
		cachedInputMatrix <- matrix()
		cachedIdentityMatrix <- matrix()

		set <- function(y)
			{
				x <<-y
				
				
			}
		get <- function() x
		setCachedInputMatrix<- function(cached) cachedInputMatrix <<- cached
		getCachedInputMatrix <- function() cachedInputMatrix
		setCachedIdentityMatrix<- function(cached) cachedIdentityMatrix<<- cached
		getCachedIdentityMatrix <- function() cachedIdentityMatrix
        list(set = set, get = get,
             setCachedInputMatrix = setCachedInputMatrix,
             getCachedInputMatrix = getCachedInputMatrix,
			 setCachedIdentityMatrix = setCachedIdentityMatrix,
             getCachedIdentityMatrix = getCachedIdentityMatrix)

	} 

## Return a matrix that is the inverse of 'x'
 
 cacheSolve <- function(x, ...) 
	{ 
		## get the matrix
		newInputMatrix <- x$get()
		## get cached input matrix if any
		cachedInputMatrix <- x$getCachedInputMatrix()
		## get cached identity matrix if any
        cachedIdentityMatrix =x$getCachedIdentityMatrix()
		## you can only use the cached value if the following conditions are met
		##		there must be an identity matrix that has been pre calculated (cachIdentityMatrix)
		##		the input matrix itself must not be null (newInputMatrix)
		##		there must exist an input matrix that was already cached through earlier calculation (cachedInputMatrix)
		##		both the new matrix to solve and the previous one that was calculated must be identical
		
		if (is.null(newInputMatrix)) return ("Matrix to calculate must not be null")
		
		if(!is.null(cachedIdentityMatrix)  & !is.null(cachedInputMatrix) & identical(newInputMatrix, cachedInputMatrix) )
		{
		

				##use what is in the cache
				message("re-using previously calculated identity matrix")
				return (cachedIdentityMatrix)

        }
		else 
		{
				## calculate the identity matrix
				message("calculating the identity matrix")
				cachedIdentityMatrix <- solve(newInputMatrix)
				cachedInputMatrix <- newInputMatrix
				x$setCachedIdentityMatrix(cachedIdentityMatrix)
				x$setCachedInputMatrix(cachedInputMatrix )
				return (cachedIdentityMatrix)
		}

	} 
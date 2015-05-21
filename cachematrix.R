makeCacheMatrix <- function(x = matrix()) { #this caches matrix
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function()
            x
      setinverse <- function(solve)
            m <<- solve
      getinverse <- function()
            m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

cacheSolve <- function(x=matrix(), ...) { #this calculates inverse of the matrix, but first pulls the cache (if it exists)
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            if(x$set() == x$get()) {}
                  message("getting cached data")
                  m <- x$getinverse()
            }
      return(m)
      }
      y <- x$get()
      x$set(y)
      m <- solve(y,...)
      x$setinverse(m)
      m
}
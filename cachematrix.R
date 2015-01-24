## This set of functions enables re-use of the solved inverse of a matrix by
## creating a lexical scope to cache the result once it is calculated.



makeCacheMatrix <- function(x = matrix()) {
  ## Takes in a matrix and returns a pseudo matrix object that can cache its
  ## calculated inverse.

  ## x defined above in the function declaration is a matrix, assumed to be
  ## square invertible and is already part of the lexical scope.

  ## This is the cached inverse in the same lexical scope.
  cachedInverse <- NULL

  ## A set of functions to get/set the above in the lexical scope
  set <- function(input) {
    ## Clear cached inverse only if matrix has really changed
    if (!identical(x, input)) {
      x             <<- input
      cachedInverse <<- NULL
    }
  }

  get <- function() {
    # return the passed-in matrix
    x
  }

  setCachedInverse <- function(inverse) {
    cachedInverse <<- inverse
  }

  getCachedInverse <- function() {
    cachedInverse
  }

  ## This is the pseudo matrix object with a set of get/set methods to get/set
  ## variables in the lexical scope
  list(
    get              = get,
    set              = set,
    getCachedInverse = getCachedInverse,
    setCachedInverse = setCachedInverse
  )
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## - Takes in pseudo matrix object created by makeCacheMatrix()
  ## - If the cached inverse is available,
  ##   - then return the cached inverse
  ##   - else calculate the inverse, cache it in the pseudo matrix object,
  ##     and return the inverse.
  inverse <- x$getCachedInverse()
  if(is.null(inverse)) {
    ## No cached inverse, so calculate. Since we explicitly only want the
    ## inverse, additional parameters to solve() is explicitly not passed in.
    inverse <- solve(x$get())
    # Cache inverse for future reference
    x$setCachedInverse(inverse)
  }
  # Return inverse
  inverse
}

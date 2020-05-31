## makeCacheMatrix contains x and inversedMtx variables for holding matrix
## and inversed matrix respectively, it also have functions for setting the
## data and getting the data for decalred variables

## returns object with setter,getter
makeCacheMatrix <- function(x = matrix()) {
  inversedMtx <- NULL
  
  set <- function(mtx) {
    x <<- mtx
    inversedMtx <<- NULL
  }
  
  get <- function()
    x
  
  setInversedMatrix <- function(inversedMatrix) inversedMtx <<- inversedMatrix
  
  getInversedMatrix <- function()
    inversedMtx
  
  list(
    set = set,
    get = get,
    setInversedMatrix = setInversedMatrix,
    getInversedMatrix = getInversedMatrix
  )
}


## returns cached inversed matrix if avialable or else will calculate and return

cacheSolve <- function(x, ...) {
  inversedMtx = x$getInversedMatrix()
  
  if (!is.null(inversedMtx)) {
    message("Getting cached inveresed matrix")
    return(inversedMtx)
  }
  
  mtx <- x$get()
  inversedMtx <- solve(mtx)
  x$setInversedMatrix(inversedMtx)
  inversedMtx
}

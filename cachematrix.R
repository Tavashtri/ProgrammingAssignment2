## makeCacheMatrix takes a matrix as paraameter and
## stores a it and it's inverse in cache. 
## It's using a list to set up the four function calls 
## 1. setMatrix stores the matrix and clears the cache of the inverse (sets it to NULL)
## 2. getMatrix retrieves the matrix
## 3. setInverseMatrix solves and stores the inverse of the matrix
## 4. getInverseMatrix returns the inver matrix.

makeCacheMatrix <- function(matrix = matrix()) {
    inverseMatrix <- NULL
    setMatrix <- function(y) {
        matrix <<- y
        inverseMatrix <<- NULL
    }
    getMatrix <- function() matrix
    setInverseMatrix <- function(solve) inverseMatrix <<- solve
    getInverseMatrix <- function() inverseMatrix
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve takes makeCacheMatrix object as parameter and checks if
## there exists a caached version of the inverse (That the cache is not NULL)
## If it finds a value it returns it. Otherwise it calculates the inverse,
## Stores it in coche through makeCacheMatrix and finally returns it.

cacheSolve <- function(matrix, ...) {
    inverseMatrix <- matrix$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- matrix$getMatrix()
    inverseMatrix <- solve(data, ...)
    matrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
}

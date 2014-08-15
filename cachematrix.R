## makeCacheMatrix function will make a special matrix object that will cache its inverse. Two global variables are used. x and m. x is the matric and m is the inverse matrix. Bith these variables are in the parent environments.
## This function has 4 functions in it: set, get, setMatrixInverse and getMatrixInverse
## set function: Takes a matrix as the argument. x is set to the input matrix. m, the inverse of the matrix is set to NULL. (x and m are in parent environments)
## get function: This simply gets the matrix that was set using set function. Nothing else.
## setMatrixInverse: This function take inverse of a matrix as an argument and sets m to the given inverse matrix.
## getmatrixInverse: This function just returns the inverse of the matric that is stored by setMatrixInverse function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(inverse) m <<- inverse
        getMatrixInverse <- function() m
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}
## cacheSolve fuction is the function that takes makeCacheMatrix as input and returns the inverse of the martix that is stored by makeCacheMatrix function.
## While returning the inverse of the matrix, it checks if the inverse is already calculated and the inverse is stored in its environment.
## If the inverse if null, it calculates the inverse, stores the newly calculated inverse globally using makeCacheMatrix$setMatricInverse function and returns the inverse of the matrix.
## If the inverse is not null, then it simply returns the inverse of the matrix that is already in the global environment.
cacheSolve <- function(x, ...) {
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}

## Here is how you run the program
## > mymat <- rbind(c(1, -1/4), c(-1/4, 1)) ==> This creates an invertible matrix and stores in variable mymat
## > w = makeCacheMatrix(mymat) ==> This creates w as makeCacheMatrix function with mymat as the input matrix.
## > cacheSolve(w)  ==> Since inverse of the matrix is not solved yet, this command solves the inverse of the matrix and outputs the matrix inverse.
## > cacheSolve(w)  ==> Since inverse of the mattic is already solved, this command will return the cached matrix inverse.
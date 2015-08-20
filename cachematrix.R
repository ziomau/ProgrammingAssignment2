## Coursera "R Programming" course (rprog-031), Assignment #rm (2, August 2015.
##
## The purpose of the following R code is to cache the inverse of a matrix
## so to avoid unnecessary time-consuming computations.
## The code leverages R lexical scoping, and is inspired by the
## "Caching the Mean of a Vector" referece example given in the course.
##
## The code contains three functions:
##  'makeCacheMatrix()': Creates a special data structure supporting matrix caching.
##  'cacheSolve()': Calculates the inverse of a matrix by exploiting caching.
##  'testCacheSolve()': a simple function for testing purposes.

## The 'makeCacheMatrix()' function creates a special "matrix",
## which is actually a list containing proper local functions to:
##  'set()': Set the data of the matrix.
##  'get()': Get the data of the matrix.
##  'setinverse()': Set the data of the inverse of the matrix.
##  'getinverse()': Get the data of the inverse of the matrix.
## The function has one input parameter that is the matrix to be stored,
## and whose inverse has to be cached.
makeCacheMatrix <- function(m = matrix()) {
  # Creates the variable for storing the inverse of the matrix
  # and initializes it to the NULL value.
  i <- NULL

  # 'set()': Local function to set the data of the matrix to be stored.
  # It has one input parameter, that is the matrix to be set.
  set <- function(y) {
    # Assigns the matrix passed as the input parameter to the matrix stored
    # by this special data structure. Please note the use of the scoping
    # assignment operator so to leverage the lexical scoping features.
    m <<- y

    # "Clears" the inverse cached matrix since a new matrix has just been set.
    i <<- NULL
  }

  # 'get()': Local function to get the data of the matrix stored.
  get <- function() {
    # Returns the matrix stored in this special data structure.
    m
  }

  # 'setinverse()': Local function to set the data of the inverse of the matrix.
  # It has one input parameter, that is the inverse of the matrix to be set.
  setinverse <- function(z) {
    # Assigns the matrix passed as the input parameter to the inverse of the matrix
    # stored by this special data structure. Again, please note the use of the
    # scoping assignment operator so to leverage the lexical scoping features.
    i <<- z
  }

  # 'getinverse()': Local function to get the data of the inverse of the matrix stored.
  getinverse <- function() {
    # Returns the inverse of matrix stored in this special data structure.
    i
  }

  # The special data structure, i.e., a list referencing the local functions
  # defined above.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve()' function returns a matrix that is the inverse of the "matrix"
## passed as input parameter. The function uses the 'solve()' function to calculate
## the inverse of the matrix. The function leverages the special data structure and
## the related functions defined in the 'makeCacheMatrix()' function in order to
## avoid unnecessary computations when possible, i.e., when cached data is available.
## The input parameter is a "special matrix", i.e., the special data structure
## created by using the makeCacheMatrix()' function.
cacheSolve <- function(sm, ...) {
  # Gets the cached inverse of the matrix from the data structure passed as input.
  im <- sm$getinverse()

  # Checks if the cached inverse matrix is not NULL, i.e., cached data is available.
  if(!is.null(im)) {
    # In such case, prints a message and returns the cached inverse matrix.
    message("Getting cached data of the inverse of the matrix.")
    return(im)
  }

  # Otherwise, no cached data is available.
  # Therefore, retrieves the matrix data stored in the special data structure
  # by using the 'get()' local function.
  matrixData <- sm$get()
  # Calculates the inverse of the matrix stored.
  inverseMatrix <- solve(matrixData, ...)
  # Stores the calculated inverse of the matrix in the special data structure
  # by using the 'setinverse()' local function.
  sm$setinverse(inverseMatrix)
  # Prints a message to the user.
  message("Calculating data of the inverse of the matrix.")
  # Returns the calculated inverse of the matrix.
  return(inverseMatrix)
}

## Simple function for testing purposes only.
## It is not part of the course assignment.
testCacheSolve <- function() {
  message("--- Test function - Begin")

  # Creates two simple invertible matrixes and prints them.
  testMatrix1 <- rbind(c(4, 3), c(3, 2))
  testMatrix2 <- rbind(c(1, 3, 3), c(1, 4, 3), c(1, 3, 4))
  message("Test matrix 1:")
  print(testMatrix1)
  readline(prompt="Press [enter] to continue")

  message("Test matrix 2:")
  print(testMatrix2)
  readline(prompt="Press [enter] to continue")

  # Initializes the special data structure with the first test matrix.
  cacheMatrix <- makeCacheMatrix(testMatrix1)

  # Calculates the inverse of the first test matrix and prints the result.
  # In this case, the "Calculating data" message should be printed because
  # this is the first time the inverse is calculated.
  temp <- cacheSolve(cacheMatrix)
  print(temp)
  readline(prompt="Press [enter] to continue")

  # Calculates again the inverse of the first test matrix and prints the results.
  # In this case, the "Getting cached data" message should be printed because
  # this is the second time the inverse is calculated on the same matrix,
  # therefore cached data should be available.
  temp <- cacheSolve(cacheMatrix)
  print(temp)
  readline(prompt="Press [enter] to continue")

  # Repeats the same steps with the second test matrix.
  # In this case, since the special data structure is initialized with a new
  # matrix, at the first calculation of the inverse the "Calculating data"
  # message should be printed, and at the subsequent attempt the "Getting
  # cached data" message should be printed.
  cacheMatrix <- makeCacheMatrix(testMatrix2)

  temp <- cacheSolve(cacheMatrix)
  print(temp)
  readline(prompt="Press [enter] to continue")

  temp <- cacheSolve(cacheMatrix)
  print(temp)

  message("--- Test function - End")
}

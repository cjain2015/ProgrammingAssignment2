## Matrix inversion is applicable to square matrices with a non zero value for determinant.
## There is no Matrix division per say but multiplying.
## Let's say divinding a matrix A by matrix B is achieved by 
## 	multiplying the matrix A with the inverse of matrix B
## Depending upon size of the matrix, it's inversion operation can be time consuming
## and hence one can get some benefit by caching the inverse of a matrix rather than computing it everytime.
## The following two functions are used to calculate and cache the inverse of a matrix.


## The first function, makeCacheMatrix is a utility function.
## It creates a list containing functions to - 
## 	"set"  			To set the value of the matrix
## 	"get"  			To get the value of the matrix
## 	"setinverseMatrix" 	To set the value of the inverse of the matrix
## 	"getinverseMatrix" 	To get the value of the inverse of the matrix
## It uses operator `<<-` to assign a value to an object in an environment different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
 	  m <- NULL
        set <- function(y) {
		## use `<<-` to assign a value to an object in an environment different from the current environment.
        	x <<- y
            m <<- NULL
        }
        get <- function() x
	  setinversematrix <- function(inverse) m <<- inverse
	  getinversematrix <- function() m 
	  list(set = set, get = get, setInversedMatrix = setinversematrix, getInversedMatrix = getinversematrix )
}

## The following function calculates the inverse of the matrix given by the above makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Eventhough it was told in the instructions that "For this assignment, assume that the matrix supplied is always invertible."
## I have written a bit more generic program for learning but it should not make any difference to the end result for a square matrix
## Otherwise - 
##   	It checks if it checks if matrix is a square matrix or not 
##  				if matrix is not square, then it skips calculating inverse and print out approriate message
##     		 then checks if determinant of matrix is zero or not. 
##				if determinant is zero, then it skips calculating inverse and print out approriate message
## 	Otherwise it calculates the inverse of the matrix and sets the value of the inverse matrix 
##    in the cache using the setinversematrix function.
##    Also Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	  ## message("within cache - solve")
        m <- x$getInversedMatrix()
        if(!is.null(m)) {
                message("Getting cached inversed matrix")
                return(m)
        }
	  ## If a matrix is not a square matrix then it doesn't have an inverse eventhough it may have a left inverse or right inverse
	  ## If a square matrix determinant is zero then it's a singular matrix and it's not invertible
	  ## non-square matrix is not in the scope of this function
	  ## Eventhough it was told in the instructions that "For this assignment, assume that the matrix supplied is always invertible."
	  ## I have written a bit more generic program for learning but it should not make any difference to the end result for a square matrix
        data <- x$get()
        if (dim(data)[1] == dim(data)[2]){       
			## when it's a square matrix

	      if (det(data) != 0){				     
			## When determinant is not zero and matrix is invertible  	

			message("Calculating and saving inverse of the matrix")
			m <- solve(data)
	        	x$setInversedMatrix(m)
        		return(m)
		}else{
			print("Matrix determinant is zero so matrix is not invertible")
		}
	  }else{
		print("Matrix is not a square matrix")
	  }		
}


## Test function that can be used to test the code when given a matrix or by using default matrix
## Case 1: Determinant is zero
## z <- matrix(1:9, 3,3)
## test(z) 
## The following output is generated - 
##       [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
## [1] "Matrix determinant is zero so matrix is not invertible"
## [1] "Matrix determinant is zero so matrix is not invertible"


## Case 2: Matrix is not a square matrix
## z <- matrix(1:6, 3, 2)
## test(z)
##       [,1] [,2]
## [1,]    1    4
## [2,]    2    5
## [3,]    3    6
## [1] "Matrix is not a square matrix"
## [1] "Matrix is not a square matrix"


## Case 3: Regular case with default argument
## test()
##       [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    3    4
## [3,]    4    1    0
## Calculating and saving inverse of the matrix
## Getting cached inversed matrix
##            [,1]        [,2]    [,3]
## [1,]  0.08333333 -0.08333333  0.2500
## [2,] -0.33333333  0.33333333  0.0000
## [3,]  0.22916667  0.02083333 -0.0625


## Case 4: Regular case but large matrix is supplied
## set.seed(3424)
## z <- matrix(rnorm(100*100), nrow=100, ncol=100)
## test(z)
## Output too large to paste here

test <- function(testMatrix = z <-(rbind( c(1,0,4), c(1,3,4), c(4,1,0) )) ){ 
    print(testMatrix) 
    mt <- makeCacheMatrix(testMatrix)
## first time call shall take longer time and will not printing "getting cached Inversed Matrix
    cacheSolve(mt)

## Second time call shall take less time and will be printing "getting cached Inversed Matrix
   cacheSolve(mt) 		
}

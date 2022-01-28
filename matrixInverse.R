# The first function, make_cache creates a matrix, which is really a list containing a function to
# 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

make_cache <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}
# the make_inverse function gets access to the object’s inverse
# matrix with the <- operator (i.e. inverse <- x$getinverse()) and returns it if found.
# if not it creates the inverse matrix and stores it in the x$setinverse object


make_inverse <- function(x,...) {
  inverse <- x$getinverse()
    if (!is.null(inverse)) {
      return(inverse)
    }
  m <- solve(x$get())
  x$setinverse(m)
}

#TEST

the_matrix <- rbind(c(0,1),c(1,1))
m <- make_cache(the_matrix)
identical(m$get(), the_matrix)
m$getinverse()
make_inverse(m)
## now make_inverse found out that the inverse in the object is NULL (<<- going one level up) 
## and replace the NULL value with the inverse of the matrix
m$getinverse()
## now let's set another matrix in the m object
m$set(rbind(sqrt(c(2,2)),c(-1,2)))
## notice that every time we enter a new matrix the inverse is turned to NULL with the <<- operator that 
## changes the m object's inverse value in the parent frame
m$getinverse()
make_inverse(m)
m$getinverse()


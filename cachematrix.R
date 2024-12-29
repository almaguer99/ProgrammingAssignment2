
#Creates a list of functions which will invoke the stored matrices
makeCacheMatrix <- function(x = matrix()) {
  nInverse<-NULL
  fSetMatrix<-function(y){
    x<<-y
    nInverse<<-NULL
  }
  fGetMatrix<-function()x
  fSetInverse<-function(nCustomInverse) nCustomInverse<<-nInverse
  fGetInverse<-function()nInverse
  
  list(
    fSetMatrix=fSetMatrix,
    fGetMatrix=fGetMatrix,
    fSetInverse=fSetInverse,
    fGetInverse=fGetInverse
  )
}


#Will look at the created function list by the previous function and then check if theris a calculated inverse or not
cacheSolve <- function(x, ...) { #The argument is the function list created by the previous function
  nInverse<-x$fGetInverse()
  if(!is.null(nInverse)){
    message("Getting the stored inverse")
    return(nInverse)
  }
  #However, if the inverse does not exists, it it has to be calculated
  nStoredMatrix<-x$fGetMatrix()
  nInverse<-solve(nStoredMatrix)
  
  #Once it is calculated, we have to store ir in order to make it availiable on cache
  x$fSetInverse(nInverse)
  nInverse
}

#lets test it
mMatrix<-(matrix(c(2,3,4,1,2,4,6,2,2),3,3,))

lStored<-makeCacheMatrix(matrix(c(2,3,4,1,2,4,6,2,2),3,3))

#See if it can calculate it
cacheSolve(lStored)

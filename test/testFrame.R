# these are just functions that can be used to perform stess tests.
# These tests will take a LONG LONG time!!!!, They will cause hundreds of jobs
# to be created in your cluster, these test should only be performed for 
# development purposes

library(Rsge)
library(nlme)

# create matrix data structure
makeMatrix <- function(nrow, ncol) { 
  matrix(1:(nrow*ncol), nrow=nrow, ncol=ncol)
}
# creates array data structure
makeArray <- function(nrow, ncol)  {
  array(1:(nrow*ncol), dim=c(nrow, ncol))
}

makeVector <- function(nrow, ncol) {
  c(1:ncol)
}

makeMilk <- function(x, y) Milk
sge.testParApply <- function(X, FUN, ..., 
                              debug=FALSE, njobs,
                              join.method=cbind, par.call="R",
			      packages=NULL,
                              savelist=NULL  
                              ) {
  num <- NULL
  if(par.call == "R") {
    num = 1
    f1  = apply
    f2  = sge.parRapply
  } else if(par.call == "C") {
    num = 2
    f1  = apply
    f2  = sge.parCapply
  } else if(par.call == "L") {
    f1  = lapply
    f2  = sge.parLapply
  } else if(par.call == "S") {
    f1  = sapply
    f2  = sge.parSapply
  }
  if(is.null(num)) {
    mr1    <- f1(X, FUN, ...)
  } else {
    mr1    <- f1(X,num,FUN, ...)
  }
#  print(join.method)
  mr1Par <- f2(X, FUN, ..., 
                          join.method=join.method,
                          njobs=njobs,
                          debug=debug,
                          packages=packages,
                          savelist=savelist
                          )
  if(debug) print(mr1Par)
  if (is.null(num)) {
    all(mr1 == unlist(mr1Par))
  } else {
    all(mr1 == mr1Par)
  }
}

#
# creates the test objects and determines
#

sge.multiTestParApply <- function(nrow, ncol, 
                                   FUN, ..., 
                                   min.jobs=1, max.jobs=nrow, 
                                   obj.call=makeMatrix,
                                   debug=FALSE) {
  x1 <- obj.call(nrow,ncol)
  v1 <- vector()
  for(i in min.jobs:max.jobs) {
    cat(paste(i, '\n'))
    v1[[i+1-min.jobs]] <- sge.testParApply(x1, FUN, ..., 
                                 njobs=i, debug=debug) 
    cat(paste(v1[[i+1-min.jobs]], '\n'))
     if(debug)  print(v1)
  }
  all(v1)
}

#
# creates a for loop that launches all of the tests
#
#

sge.massiveTestParApply <- function(r1, r2, c1, c2, fun, ..., debug=FALSE) {
 if(c1 < 2 || c2 < 2) {
   warning("Number must be greater than 1")
   return(NULL)
 }
 v1 <- vector()
 v2 <- vector()
 for(i in r1:r2) {
# having only one column is not supported 
  for(j in c1:c2) {
    cat(paste(i,j,'\n', sep="|"))
    v1[[j+1-c1]] <- sge.multiTestParApply(i,j,fun, ..., debug=debug)
  }
  v2[[i+1-r1]] <- all(v1)
 } 
 if(debug) cat(v2)
 all(v2)
}

func1 <- function(x) x+7
func2 <- function(x,y,z) x + y + z
func3 <- function(x, y) {
  x1 <- Milk
  x + x1$protein[[1]] + y
}
func4 <- function(x) x+y
func5 <- function(x) x[[1]][[1]] <- x[[1]][[1]] + y +z
debug=FALSE
#sge.massiveTestParApply(1, 3, 2, 3, func1)
#sge.massiveTestParApply(2, 3, 4, 5 , mean, join.method=c)
#sge.massiveTestParApply(57,59, 556, 559, func2, 4, 5, max.jobs=5)
#sge.massiveTestParApply(102,116,214,217, 
#                        func2, 4, 5, 
#                        min.jobs=5, max.jobs=10, debug=debug)
#sge.massiveTestParApply(1034,1034,8007,8008, 
#                        func2, 4, 5, 
#                        min.jobs=27, max.jobs=29, debug=debug,
#                        obj.call=makeArray, par.call="R")
#sge.massiveTestParApply(37,40,1001,1007, 
#                       func2, 4, 5, 
#                        min.jobs=5, max.jobs=9, debug=debug,
#                        obj.call=makeArray, par.call="C")
#sge.massiveTestParApply(9,18,4,5, 
#                        mean, 4, 5, 
#                        min.jobs=1, max.jobs=5, debug=debug,
#                        obj.call=makeArray, par.call="C", join.method=c)
#sge.massiveTestParApply(2,2,1000,1015, 
#                        func2, 4, 5, 
#                        min.jobs=30, max.jobs=37, debug=debug,
#                        obj.call=makeVector, par.call="L", 
#                        join.method=c)
#sge.massiveTestParApply(2,2,1899,1902, 
#                        func2, 4, 5, 
#                        min.jobs=3, max.jobs=6, debug=debug,
#                        obj.call=makeVector, par.call="S", 
#                        join.method=c)
#sge.massiveTestParApply(2,2, 1899, 1899,
#                       func3, 3, packages=c("nlme"),
#                       min.jobs=3,max.jobs=5, debug=debug,
#                       obj.call=makeVector, par.call="S",
#                       join.method=c)
y <- 10
z <- 20
#sge.massiveTestParApply(2,2, 9, 9,
#                       func4, savelist=c("y"),
#                       min.jobs=3,max.jobs=5, debug=debug,
#                       obj.call=makeVector, par.call="S",
#                       join.method=c)
sge.massiveTestParApply(2,2, 9, 9,
                       func5, savelist=c("y", "z"),
                       min.jobs=3,max.jobs=3, debug=TRUE,
                       obj.call=makeMilk, par.call="S",
                       join.method=c)

# I still need to test savelist and package import
# also asynchronous calls
# still need to test data frames, Milk is a data frame

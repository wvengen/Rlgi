#
# This test was written to test the global data functionality.
# Everything is now stored in a single file of the name RsgeNUM-GLOBAL
# only the X varialbe is stored seperately for each task.
# 
# There are still a few things to verify: 
# 1. That I can always reset X
# 2. That the program crashes if there are variables named X
# additional tests on environments are stored in testEnv.R
#
# These test cases are all covered in testEnv.R, I will verify this 
# and remove them evantually
#
library(Rsge)
sge.options("sge.remove.files=TRUE")
#options(sge.debug='TRUE')
ncols <- 5000
nrows <- 500
# huge global value
GM <- matrix(data = runif(n=nrows*ncols,1,1000),
            ncol=ncols,
            nrow=nrows,
            dimnames=list(paste(sep="","R",1:nrows),paste(sep="","C",1:ncols)))
#this shows the size to store the large test data, all of the Rsge files should be
#of similar size
save(list=c("GM"), file="SIZE-TEST")

# X value , value to be split, value used to test with lapply
v1 <- 1:10
# matrix value used to test with sapply and apply
m1 <- matrix(rnorm(5), ncol=1) 

#global value referenced in function
G1=100

f1 <- function(x, y) {
  GM[[1]] + G1 + x + y
}

f2 <- function(x) {
  GM[[1]] + G1 + x
}


sl=c("GM", "G1")

#testing that savelist is saving data as global
r1    <- lapply(X=v1, FUN=f1, 4) 
sge.options(sge.save.global=TRUE)
r1Par <- sge.parLapply(v1, f1, 4, njobs=3, file.prefix="TEST1")
r2    <- lapply(X=v1, FUN=f2)
r2Par <- sge.parLapply(v1, f2, global.savelist=sl, njobs=3, file.prefix="TEST2")

f3 <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9) {
  x1 + mean(x2) + x3[[1]]/8 + G1 + x4 +x5 + x6 + x7+ x8 + x9 + GM[[1]] 
  #x1 + mean(x2) + G1 + x4 +x5 + x6 + x7+ x8 + x9
}

sge.options(sge.save.global=FALSE)
x1 <- 1:5
x2 <- matrix(rnorm(5), ncol=1)
r3    <- lapply(v1, f3, x1, x2, 3, 4, 5, 6, 7, 8) 
r3Par <- sge.parLapply(v1, f3, x1, x2, 3, 4, 5, 6, 7, 8, global.savelist=sl, njobs=3, file.prefix="TEST2")

#X <- "TEST"
#savelist2=c("GM", "G1", "X")
#tryCatch(
##  {
#    r4Par <- sge.parLapply(X=v1, FUN=f2, savelist=savelist2, njobs=3, file.prefix="TEST4")
##    options(X_ERR="FALSE")
#  }, error = function(ex) {
#    options(X_ERR="TRUE")
#  }
#)#end tryCatch
#
m1 <- array(1:20, dim=c(4,5))

sge.options(sge.save.global=TRUE)
#test row applies with argument and global savelist
mr1    <- apply(X=m1,MARGIN=1, FUN=f1, 1)
mr1Par <- sge.parRapply(X=m1, FUN=f1 ,1, njobs = 2, file.prefix="TEST5")
#test column apply with no argument and global savelist
mc1    <- apply(X=m1, MARGIN=2, FUN=f2)
mc1Par <- sge.parCapply(X=m1, FUN=f2, njobs=5, file.prefix="TEST6")
####

#test that I can pass globals to CApply
#After this test, the filw sizes for GLOBAL should be much bigger
ml1   <- apply(X=m1, MARGIN=1, FUN=f1, 1) 
mlp1  <- sge.apply(X=m1, MARGIN=1, FUN=f1, 1, njobs=3, file.prefix="TEST7" )
ml2   <- apply(X=m1, MARGIN=2, FUN=f1, 1)
mlp2  <- sge.apply(X=m1, MARGIN=2, FUN=f1, 1, njobs=3, file.prefix="TEST8" )

print("Reviewing Test Results, any FALSE returns inficate test failure")
print("LAPPLY TESTS")
print("TEST1")
all(r1 == unlist(r1Par))
print("TEST2")
all(r2 == unlist(r2Par))
print("TEST3")
all(r3 == unlist(r3Par))
#print("TEST X_ERR")
#getOption("X_ERR")
print("APPLY TESTS")
print("TEST5")
all(mr1==mr1Par)
print("TEST6")
all(mc1==mc1Par)
print("TEST7")
all(ml1 == mlp1)
print("TEST8")
all(ml2 == mlp2)

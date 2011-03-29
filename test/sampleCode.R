library(Rsge)
ncols <- 5000 
nrows <- 500
d <- matrix(data = runif(n=nrows*ncols,1,1000),
            ncol=ncols,
            nrow=nrows,
            dimnames=list(paste(sep="","R",1:nrows),paste(sep="","C",1:ncols)))
#
# returns a vector
#
rMean    <- apply(d,1,mean)
rMeanPar <- sge.parRapply(d, mean, njobs=3, join.method=c)
all(rMean == rMeanPar)
cMean    <- apply(d,2,mean)
cMeanPar <- sge.parCapply(d, mean, njobs=3, join.method=c)
all(cMean == cMeanPar)
#
# returns a vector and has a function argument
#
rMean2    <- apply(d,1,mean,trim=0.02)
rMean2Par <- sge.parRapply(d, mean, trim=0.02, njobs=3, join.method=c)
all(rMean2 == rMean2Par)
cMean2    <- apply(d,2,mean,trim=0.2)
cMean2Par <- sge.parCapply(d, mean, trim=0.2,  njobs=3, join.method=c)
all(cMean2 == cMean2Par)
#
# returns a matrix
#
dLog     <-  apply(d,1,log)
dLogPar  <-  sge.parRapply(d, log, njobs=5, join.method=cbind)
all(dLog == dLogPar)
#
# introduce negative numbers and produce warnings
#

d[3,6] <-d[5,50]<- d[3,10]<- d[4,15] <-  -1
dLog   <-apply(d,1,log)
dLogPar  <-  sge.parRapply(d, log, njobs=5, join.method=cbind)
all(dLogPar == dLog)

#
# returns a list
#
response <- runif(ncols,1,1000)
func1 <- function(x,y) {lm(y ~ x,data=data.frame(x=x,y=y))}

dFitR    <- apply(d,1,func1,y=response)
dFitRPar <- sge.parRapply(d, func1,y=response, njobs = 3, join.method=c)
dFitC    <- apply(d,2,func1,y=response[1:nrows])
dFitCPar <- sge.parCapply(d,func1,y=response[1:nrows], njobs = 5, join.method=c)

#
# loop over the list and produce a vector
#
func2 <- function(x) x$fitted.values/x$residuals

result   <- lapply(dFitR,func2)
result2  <- sge.parLapply( dFitRPar,func2, njobs = 3)
ra1 <- array()
for (i in 1:length(result)) {
  ra1[[i]] <-  all(result[[i]] == result2[[i]])
}
all(ra1)
resultC  <- lapply(dFitC,func2)
resultC2 <- sge.parLapply(dFitC,func2, njobs = 4)
ra2 <- array()
for (i in 1:length(resultC)) {
  ra2[[i]] <-  all(resultC[[i]] == resultC2[[i]])
}
all(ra2)

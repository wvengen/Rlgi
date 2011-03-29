\name{sge.parApply}
\alias{sge.parParApply}
\alias{sge.parRapply}
\alias{sge.parCapply}
\alias{sge.parLapply}
\alias{sge.parSapply}
\alias{sge.apply}
\title{Method used to integrate apply, lapply, sapply fcuntionality with qsub.}
\description{
  applies each row/column of a matrix or each element of a list to a function.
}

\usage{
  sge.apply   (X, MARGIN, FUN, ..., 
               join.method=cbind,
               njobs,batch.size=getOption('sge.block.size'),
               packages=NULL, global.savelist=NULL,function.savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=getOption("sge.trace"), debug=getOption("sge.debug"),
               file.prefix=getOption("sge.file.prefix"))
  sge.parRapply(X, FUN, ..., join.method=cbind, 
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=getOption("sge.trace"), debug=getOption("sge.debug"),
               file.prefix=getOption("sge.file.prefix"))
 
  sge.parCapply(X, FUN, ..., join.method=cbind, 
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=getOption("sge.trace"), debug=getOption("sge.debug"),
               file.prefix=getOption("sge.file.prefix"))
 
  sge.parLapply(X, FUN, ..., join.method=c, 
               njobs, batch.size=getOption('sge.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("sge.use.cluster"),
               trace=getOption("sge.trace"), debug=getOption("sge.debug"),
               file.prefix=getOption("sge.file.prefix")) 

  sge.parSapply(X, FUN, ...,  
                USE.NAMES=TRUE, simplify=TRUE,
                join.method=c, 
                njobs, batch.size=getOption('sge.block.size'),
                packages=NULL, global.savelist=NULL, function.savelist=NULL,
                cluster=getOption("sge.use.cluster"),
                trace=getOption("sge.trace"), debug=getOption("sge.debug"),
                file.prefix=getOption("sge.file.prefix"))
  sge.parParApply(X, FUN, ...,
               join.method=cbind,
               njobs,
               batch.size=getOption('sge.block.size'),
               trace=getOption("sge.trace"),
               packages=NULL,
               global.savelist=NULL, function.savelist=NULL,
               debug=getOption("sge.debug"),
               file.prefix=getOption('sge.file.prefix'),
               apply.method
              )
}

\arguments{
  \item{X}{Object to be applied to function (matrix, data.frame, list, array, or vector supported)}
  \item{MARGIN}{Used by sge.par to determime if (1) sge.Rapply or (2) sge.Capply should be called.}
  \item{FUN}{Function to be applied to object}
  \item{\dots}{Additional arguments to be applied to function}
  \item{njobs}{Number of parallel jobs to use}
  \item{join.method}{Function used to merge results from each job.}
  \item{batch.size}{Number of rows to include in parallel job if njobs is excluded.}
  \item{global.savelist}{Character vector giving the names of variables from the global environemnt that should be imported. If sge.save.global is set to TRUE, then this will clear the global environment. To set the global environment to be empty, use vector()}
  \item{function.savelist}{Character vector giving the variables to save from the local environment. <Passing a vector() will cause the local function to be empty, any value here will remove any non-listed values }
  \item{packages}{List of library packages to be loaded by each worker
    process before computation is started. }
  \item{cluster}{determines if the job should be submitted to the cluster or run locally (default:submit to cluster, TRUE)}
  \item{trace}{Prints some information about job submission. (TRUE|FALSE)}
  \item{file.prefix}{Prefix for the data files used to pass information between nodes}
  \item{debug}{Prints debug level info about jobs. (TRUE|FALSE)}
  \item{USE.NAMES}{Determines if the sapply use names functionality should be used}
  \item{simplify}{Detemines if the sapply simplify functionality should be used}
  \item{apply.method}{tells apply if it should apply as a list or matrix}
}
\details{

  sge.parApply applies the function argument to either the rows, or elements of x. (depending on the value of apply.method)

    The arguments \code{packages} and \code{savelist} can be used to
  properly initialize the worker processes.

  sge.parCapply computes the transpose of X and uses it for the arguments of parRapply.

  sge.parLapply and sge.parSapply use lapply, sge.parCapply, and sge.parRapply use apply (Margin=1) 


}
\value{
Returns an object whose type is determined by the join.method. This object should be equivalent to the object that would be returned by an equivalent call to apply, sapply, or lapply. 
}

\author{Dan Bode \email{dbode@univaud.com}}
\examples{
\dontrun{  
 # there are tons of examples (actually my crude unit tests)
 # in the tests directory of this package. These contain tons of 
 # examples of how to dop everything
 # I also recommend taking a look at the ENVIRONMENTS document, since 
 # the environment is being handled specially. 

 # basic lapply call
 sge.parLapply(c(1:10), function(x) x + 1)
 # run locally
 options(sge.use.cluster=FALSE)
 sge.parLapply(c(1:10), function(x) x + 1)
 options(sge.use.cluster=TRUE)
 #pass extra arguments
 sge.parLapply(c(1:10), function(x,y) x + y, 3) 

 # work with martices
 m2 <- array(1:20, dim=c(4,5))
 # apply rows, split into 3 jobs
 sge.parRapply(m2, function(x,y) x + y, 3, njobs=3)
 # bind results with c, since we are changing dimensions
 sge.parRapply(m2, mean, njobs=3, join.method=c)

 # working with GLOBAL variables
 GLOBAL1 = 1
 # rather this variable is saved by default depends on the value of sge.save.global
 sge.options(sge.save.global=TRUE)
 sge.parLapply(c(1:10), function(x) x + GLOBAL1)
 sge.options(sge.save.global=FALSE)
 sge.parLapply(c(1:10), function(x) x + GLOBAL1, global.savelist=c("GLOBAL1"))
}
}

\keyword{interface}

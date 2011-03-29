\name{lgi.parApply}
\alias{lgi.parParApply}
\alias{lgi.parRapply}
\alias{lgi.parCapply}
\alias{lgi.parLapply}
\alias{lgi.parSapply}
\alias{lgi.apply}
\title{Method used to integrate apply, lapply, sapply fcuntionality with qsub.}
\description{
  applies each row/column of a matrix or each element of a list to a function.
}

\usage{
  lgi.apply   (X, MARGIN, FUN, ..., 
               join.method=cbind,
               njobs,batch.size=getOption('lgi.block.size'),
               packages=NULL, global.savelist=NULL,function.savelist=NULL,
               cluster =getOption("lgi.use.cluster"),
               trace=getOption("lgi.trace"), debug=getOption("lgi.debug"),
               file.prefix=getOption("lgi.file.prefix"),
               application=getOption("lgi.application"))
  lgi.parRapply(X, FUN, ..., join.method=cbind, 
               njobs, batch.size=getOption('lgi.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("lgi.use.cluster"),
               trace=getOption("lgi.trace"), debug=getOption("lgi.debug"),
               file.prefix=getOption("lgi.file.prefix"),
               application=getOption("lgi.application"))
 
  lgi.parCapply(X, FUN, ..., join.method=cbind, 
               njobs, batch.size=getOption('lgi.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("lgi.use.cluster"),
               trace=getOption("lgi.trace"), debug=getOption("lgi.debug"),
               file.prefix=getOption("lgi.file.prefix"),
               application=getOption("lgi.application"))
 
  lgi.parLapply(X, FUN, ..., join.method=c, 
               njobs, batch.size=getOption('lgi.block.size'),
               packages=NULL, global.savelist=NULL, function.savelist=NULL,
               cluster =getOption("lgi.use.cluster"),
               trace=getOption("lgi.trace"), debug=getOption("lgi.debug"),
               file.prefix=getOption("lgi.file.prefix"),
               application=getOption("lgi.application"))

  lgi.parSapply(X, FUN, ...,  
                USE.NAMES=TRUE, simplify=TRUE,
                join.method=c, 
                njobs, batch.size=getOption('lgi.block.size'),
                packages=NULL, global.savelist=NULL, function.savelist=NULL,
                cluster=getOption("lgi.use.cluster"),
                trace=getOption("lgi.trace"), debug=getOption("lgi.debug"),
                file.prefix=getOption("lgi.file.prefix"),
                application=getOption("lgi.application"))
  lgi.parParApply(X, FUN, ...,
               join.method=cbind,
               njobs,
               batch.size=getOption('lgi.block.size'),
               trace=getOption("lgi.trace"),
               packages=NULL,
               global.savelist=NULL, function.savelist=NULL,
               debug=getOption("lgi.debug"),
               file.prefix=getOption('lgi.file.prefix'),
               apply.method=2,
               application=getOption("lgi.application")
              )
}

\arguments{
  \item{X}{Object to be applied to function (matrix, data.frame, list, array, or vector supported)}
  \item{MARGIN}{Used by lgi.par to determime if (1) lgi.Rapply or (2) lgi.Capply should be called.}
  \item{FUN}{Function to be applied to object}
  \item{\dots}{Additional arguments to be applied to function}
  \item{njobs}{Number of parallel jobs to use}
  \item{join.method}{Function used to merge results from each job.}
  \item{batch.size}{Number of rows to include in parallel job if njobs is excluded.}
  \item{global.savelist}{Character vector giving the names of variables from the global environemnt that should be imported. If lgi.save.global is set to TRUE, then this will clear the global environment. To set the global environment to be empty, use vector()}
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
  \item{application}{LGI application to submit to}
}
\details{

  lgi.parApply applies the function argument to either the rows, or elements of x. (depending on the value of apply.method)

    The arguments \code{packages} and \code{savelist} can be used to
  properly initialize the worker processes.

  lgi.parCapply computes the transpose of X and uses it for the arguments of parRapply.

  lgi.parLapply and lgi.parSapply use lapply, lgi.parCapply, and lgi.parRapply use apply (Margin=1) 


}
\value{
Returns an object whose type is determined by the join.method. This object should be equivalent to the object that would be returned by an equivalent call to apply, sapply, or lapply. 
}

\author{Dan Bode \email{dbode@univaud.com}}
\author{W. van Engen \email{wvengen@nikhef.nl}}
\examples{
\dontrun{  
 # there are tons of examples (actually my crude unit tests)
 # in the tests directory of this package. These contain tons of 
 # examples of how to dop everything
 # I also recommend taking a look at the ENVIRONMENTS document, since 
 # the environment is being handled specially. 

 # basic lapply call
 lgi.parLapply(c(1:10), function(x) x + 1)
 # run locally
 options(lgi.use.cluster=FALSE)
 lgi.parLapply(c(1:10), function(x) x + 1)
 options(lgi.use.cluster=TRUE)
 #pass extra arguments
 lgi.parLapply(c(1:10), function(x,y) x + y, 3) 

 # work with martices
 m2 <- array(1:20, dim=c(4,5))
 # apply rows, split into 3 jobs
 lgi.parRapply(m2, function(x,y) x + y, 3, njobs=3)
 # bind results with c, since we are changing dimensions
 lgi.parRapply(m2, mean, njobs=3, join.method=c)

 # working with GLOBAL variables
 GLOBAL1 = 1
 # rather this variable is saved by default depends on the value of lgi.save.global
 lgi.options(lgi.save.global=TRUE)
 lgi.parLapply(c(1:10), function(x) x + GLOBAL1)
 lgi.options(lgi.save.global=FALSE)
 lgi.parLapply(c(1:10), function(x) x + GLOBAL1, global.savelist=c("GLOBAL1"))
}
}

\keyword{interface}

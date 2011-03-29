\name{sge.get.result}
\alias{sge.get.result}
\alias{sge.list.get.result}
\title{Returns the results for a job that has been executed in the SGE environment}
\description{
  Used to retrieve stored results that were written by the worker processes. 
}
\usage{
sge.get.result(sge.fname, jobid)
sge.list.get.result(list, ...) 
}
\arguments{
  \item{sge.fname}{File location where the worker process results are stored}
  \item{jobid}{Job id of the job whose results we are returning}
  \item{list}{List that contains the jobid and filename}
  \item{\dots}{also passes sge.fname, jobid, and remove arguments to sge.get.result}
}
\details{
  If the list version is used, then the list must have the named elements 
  filename and jobid. 
 
  Rsge integrates with SGE by using the R functions load/store to write 
  files to to a shared file system accessible by both the worker processes 
  and the submission process. 

  Every node that runs will store its results in a unique file specified 
  by the options sge.file.prefix and sge.ret.ext. This function excepts 
  the results filename to be passed as an option.

    sge.file.prefixuniquestring[SGE\_TASK\_ARRAY\_ID].sge.ret.ext

  This funciton is used internally by sge.parApply, and should be called by the user to gather results from calls to sge.submit.
} 
\value{
  Returns results from the worker processes.
}
 
\author{Dan Bode \email{dbode@univaud.com}}


\seealso{\code{\link[Rsge]{sge.submit}} }

\examples{
}
\keyword{interface}

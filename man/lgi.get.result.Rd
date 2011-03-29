\name{lgi.get.result}
\alias{lgi.get.result}
\alias{lgi.list.get.result}
\title{Returns the results for a job that has been executed in an LGI environment}
\description{
  Used to retrieve stored results that were written by the worker processes. 
}
\usage{
lgi.get.result(lgi.fname, jobid)
lgi.list.get.result(list, ...) 
}
\arguments{
  \item{lgi.fname}{File location where the worker process results are stored}
  \item{jobid}{Job id of the job whose results we are returning}
  \item{list}{List that contains the jobid and filename}
  \item{\dots}{also passes lgi.fname, jobid, and remove arguments to lgi.get.result}
}
\details{
  If the list version is used, then the list must have the named elements 
  filename and jobid. 
 
  Rlgi integrates with LGI by using the R functions load/store to write 
  files to to a shared file system accessible by both the worker processes 
  and the submission process. 

  Every node that runs will store its results in a unique file specified 
  by the options lgi.file.prefix and lgi.ret.ext. This function excepts 
  the results filename to be passed as an option.

    lgi.file.prefixuniquestring[SGE\_TASK\_ARRAY\_ID].lgi.ret.ext

  This funciton is used internally by lgi.parApply, and should be called by the user to gather results from calls to lgi.submit.
} 
\value{
  Returns results from the worker processes.
}
 
\author{Dan Bode \email{dbode@univaud.com}}


\seealso{\code{\link[Rlgi]{lgi.submit}} }

\examples{
}
\keyword{interface}

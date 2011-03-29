\name{lgi.result}
\alias{lgi.result}
\title{Returns the results for a job that has been executed in an LGI environment}
\description{
  Used to retrieve stored results that were written by the worker processes. 
}

\usage{
lgi.get.result(jobid, fname=NA,
               debug=getOption("lgi.debug"), trace=getOption("lgi.trace"))
}
\arguments{
  \item{jobid}{Job id of the job whose results we are returning}
  \item{fname}{File location where the worker process results are stored (optional)}
  \item{debug}{Runs at debug level}
  \item{trace}{Runs at trace level}
}
\details{
  Rlgi integrates with LGI by using the R functions load/store to write 
  files to to a shared file system accessible by both the worker processes 
  and the submission process. 

  Every node that runs will store its results in a unique file specified 
  by the option \code{lgi.file.prefix}. This function either uses the results
  filename as specified, or else searches the job's repository for the result.

  This function is used internally by \code{lgi.parApply}, and should be called by the user to gather results from calls to \code{lgi.submit}.
} 
\value{
  Returns results from the worker processes.
}
 
\author{Dan Bode \email{dbode@univaud.com}}
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.submit}}
}

\keyword{interface}

\name{lgi.job.status}
\alias{lgi.job.status}
\title{Determines the state of jobs}
\description{
Used to determine if a job is running. 
}
\usage{
lgi.job.status(jobid)
}
\arguments{
\item{jobid}{ID of the job to be checked}
}
\details{
This method checks the LGI_qstat command to see if the specified job is still running. 
It returns 0 if the specified job has completed. 
The return will be the return code for LGI_qstat \-j JOBID.
}
\value{
  0 specifies job/completion 
  Other returns are forwarded from LGI_qstat \-j JOBID. 
}
 
\author{Dan Bode \email{dbode@univaud.com}}

\seealso{\code{\link[Rlgi]{lgi.submit}} \code{\link[Rlgi]{lgi.list.get.result}} }

\examples{
}
\keyword{interface}

\name{sge.job.status}
\alias{sge.job.status}
\title{Determines the state of jobs}
\description{
Used to determine if a job is running. 
}
\usage{
sge.job.status(jobid, qacct=getOption("sge.use.qacct"))
}
\arguments{
\item{jobid}{ID of the job to be checked}
\item{qacct}{Specifies if qacct should be used to determine the status of completed jobs}
}
\details{
This method checks the qstat command to see if the specified job is still running. 
It returns 0 if the specified job has completed. 
If qacct is set to "TRUE", then the program will return the return code of qacct \-j JOBID (0 for success, non zero for error) 
  Otherwise, the return will be the return code for qstat \-j JOBID.
}
\value{
  0 specifies job/completion 
  Other returns are forwarded from qacct \-j JOBID or qstat \-j JOBID. 
}
 
\author{Dan Bode \email{dbode@univaud.com}}

\seealso{\code{\link[Rsge]{sge.submit}} \code{\link[Rsge]{sge.list.get.result}} }

\examples{
}
\keyword{interface}

\name{sge.get.jobid}
\alias{sge.get.jobid}
\title{Returns the jobid from a qsub call.}
\description{
This function parses the output from a qsub call and returns the job id.
}
\usage{
sge.get.jobid(result)
}
\arguments{
  \item{result}{Resulting output from qsub call}
}
\details{
  This function parses the output from a qsub call and returns the job id.
}
\value{
  integer id of job.
}
\author{Dan Bode}
\keyword{interface}

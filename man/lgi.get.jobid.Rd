\name{lgi.get.jobid}
\alias{lgi.get.jobid}
\title{Returns the jobid from a qsub call.}
\description{
This function parses the output from a qsub call and returns the job id.
}
\usage{
lgi.get.jobid(result)
}
\arguments{
  \item{result}{Resulting output from LGI_qsub call}
}
\details{
  This function parses the output from an LGI_qsub call and returns the job id.
}
\value{
  integer id of job.
}
\author{Dan Bode}
\keyword{interface}

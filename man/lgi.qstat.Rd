\name{lgi.qstat}
\alias{lgi.qstat}
\title{Queries for LGI project server for job information}
\description{
  Return job information 
}

\usage{
lgi.qstat(jobid=NULL,
          debug=getOption("lgi.debug"), trace=getOption("lgi.trace"))
}
\arguments{
  \item{jobid}{LGI job id to query, or none for all jobs accessible to the user}
  \item{debug}{Runs at debug level}
  \item{trace}{Runs at trace level}
}
\value{
  LGI job status XML. This can be passed to \code{lgi.job.id}, \code{lgi.job.status}
  and \code{lgi.job.repourl} to obtain information.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}}
  \code{\link[Rlgi]{lgi.job.status}}
  \code{\link[Rlgi]{lgi.job.repourl}}
  \code{\link[Rlgi]{lgi.qsub}}
}

\keyword{interface}

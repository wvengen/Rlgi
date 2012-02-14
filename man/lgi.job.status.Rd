\name{lgi.job.status}
\alias{lgi.job.status}
\title{Returns the job status from an LGI result}
\description{
  Return the LGI job status from the output of an LGI command
  like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.status(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  LGI status of job as string.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.title}};
  \code{\link[Rlgi]{lgi.job.repourl}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

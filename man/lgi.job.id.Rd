\name{lgi.job.id}
\alias{lgi.job.id}
\title{Returns the jobid from an LGI result}
\description{
  Return the LGI job id from the output of an LGI command
  like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.id(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  integer id of job.
}

\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.status}},
  \code{\link[Rlgi]{lgi.job.repourl}};
  \code{\link[Rlgi]{lgi.job.title}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}
\keyword{interface}

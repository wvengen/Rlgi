\name{lgi.job.repourl}
\alias{lgi.job.repourl}
\title{Returns the job repository location from an LGI result}
\description{
  Return the LGI job repository location from the output of an LGI
  command like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.repourl(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  LGI repository location of job as string.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.status}};
  \code{\link[Rlgi]{lgi.job.title}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

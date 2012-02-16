\name{lgi.job.title}
\alias{lgi.job.title}
\title{Returns the job title from an LGI result}
\description{
  Return the LGI job title from the output of an LGI command
  like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.title(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  LGI title of job as string.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.status}},
  \code{\link[Rlgi]{lgi.job.repourl}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

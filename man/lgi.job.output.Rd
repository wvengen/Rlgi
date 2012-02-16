\name{lgi.job.output}
\alias{lgi.job.output}
\title{Returns the job output from an LGI result}
\description{
  Return the LGI job output from the output of an LGI command
  like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.output(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  LGI output of job as string.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.input}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

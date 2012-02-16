\name{lgi.job.input}
\alias{lgi.job.input}
\input{Returns the job input from an LGI result}
\description{
  Return the LGI job input from the output of an LGI command
  like \code{lgi.qsub} or \code{lgi.qstat}.
}

\usage{
lgi.job.input(xml)
}
\arguments{
  \item{xml}{Resulting output from LGI qsub or qstat call}
}
\value{
  LGI input of job as string.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.output}};
  \code{\link[Rlgi]{lgi.qsub}},
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

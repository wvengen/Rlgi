\name{lgi.resource}
\alias{lgi.resource.repourl}}
\title{LGI resource functions}
\description{
  When Rlgi is loaded not on your own computer, but on a resource node, it
  acts slightly different. It is currently not possible to query or submit
  jobs in this case, but file access is.
}

\usage{
lgi.resource.repourl()
}
\value{
  The repository url for the job currently running is returned.
}
\details{
  The result can be used as an argument to \code{lgi.file.get},
  \code{lgi.file.put} and \code{lgi.file.list}.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.file.get}}
  \code{\link[Rlgi]{lgi.file.put}}
  \code{\link[Rlgi]{lgi.file.list}}
  \code{\link[Rlgi]{lgi.job.repourl}}
}

\keyword{interface}

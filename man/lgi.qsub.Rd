\name{lgi.qsub}
\alias{lgi.qsub}
\title{Submits R code to an LGI cluster}
\description{
  A snippet of R code is submitted to an LGI cluster; the function
  supplied is just used as standard input to the R program.
}

\usage{
lgi.qsub(rcode, application, files=c(),
          debug=getOption("lgi.debug"), trace=getOption("lgi.trace"))
}
\arguments{
  \item{rcode}{R function to execute on LGI cluster}
  \item{application}{LGI application for running R programs}
  \item{files}{paths of files to upload before running the R job remotely}
  \item{debug}{Runs at debug level}
  \item{trace}{Runs at trace level}
}
\value{
  LGI job status XML. This can be passed to \code{lgi.job.id}, \code{lgi.job.status}
  and \code{lgi.job.repourl} to obtain information.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.id}},
  \code{\link[Rlgi]{lgi.job.status}},
  \code{\link[Rlgi]{lgi.job.repourl}};
  \code{\link[Rlgi]{lgi.qstat}}.
}

\keyword{interface}

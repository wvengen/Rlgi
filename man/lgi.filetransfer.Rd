\name{lgi.filetransfer}
\alias{lgi.file.get}
\alias{lgi.file.put}
\alias{lgi.file.list}
\title{Uploads or downloads files to an LGI repository}
\description{
  LGI jobs usually retrieve files from their repository before starting, and
  store results when finished. These functions access this repository.
}

\usage{
lgi.file.get(repo, files, trace=getOption('lgi.trace'))
lgi.file.put(repo, files, trace=getOption('lgi.trace'))
lgi.file.list(repo)
}
\arguments{
  \item{repo}{Repository to access, can be obtained by \code{lgi.job.repourl}; no trailing slash}
  \item{files}{List of files to operate on}
  \item{trace}{Runs at trace level}
}
\value{
  For \code{lgi.file.list} a named list is returned with filenames and size and timestamp of each.
}
\details{
  \code{lgi.file.get} retrieves files from the repository and \code{lgi.file.put} stores
  them. \code{lgi.file.list} returns the files that are available in the repository.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.repourl}}
  \code{\link[Rlgi]{lgi.resource.repourl}}
}

\keyword{interface}

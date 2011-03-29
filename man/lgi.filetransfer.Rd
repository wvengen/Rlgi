\name{lgi.filetransfer}
\alias{lgi.filetransfer}
\alias{lgi.file.get}
\alias{lgi.file.put}
\title{Uploads or downloads files to an LGI repository}
\description{
  LGI jobs usually retrieve files from their repository before starting, and
  store results when finished. These functions access this repository.
}

\usage{
lgi.filetransfer(action, repo, files, debug=getOption("lgi.debug"), trace=getOption("lgi.trace"))
lgi.file.get(repo, files)
lgi.file.put(repo, files)
}
\arguments{
  \item{action}{LGI_filetransfer action to execute ("download", "upload", "list" or "delete")}
  \item{repo}{Repository to access, can be obtained by \code{lgi.job.repourl}}
  \item{files}{List of files to operate on}
  \item{debug}{Runs at debug level}
  \item{trace}{Runs at trace level}
}
\value{
  LGI filetransfer status XML.
}
\details{
  \code{lgi.file.get} retrieves files from the repository and \code{lgi.file.put} stores
  them. \code{lgi.filetransfer} is a direct wrapper for LGI_filetransfer. You shouldn't
  need to use it directly.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.job.repourl}}
}

\keyword{interface}

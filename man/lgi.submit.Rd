\name{lgi.submit}
\alias{lgi.submit}
\alias{lgi.run}
\title{Performs an asynchronous submission of a function to the remote node}
\description{
  Used to submit R tasks to an LGI cluster.
}
\usage{
lgi.submit(func, ..., 
           application=getOption("lgi.application"),
           global.savelist=NULL,
           function.savelist=NULL,
           packages=NULL, 
           debug=getOption("lgi.debug"), trace=getOption("lgi.trace")
           file.prefix=getOption('lgi.file.prefix')
           )
lgi.run(...)
}
\arguments{
  \item{func}{Function to be executed remotely}
  \item{...}{Arguments to be passed to the function. For \code{lgi.run}, arguments to be passed to \code{lgi.submit}}
  \item{application}{LGI application to submit to}
  \item{global.savelist}{Character vector giving the names of variables from the global environemnt th
at should be imported. If \code{lgi.save.global} is set to TRUE, then this will clear the global environment.
 To set the global environment to be empty, use vector()}
  \item{function.savelist}{Character vector giving the variables to save from the local environment.
Passing any arguments here will cause the function environment to be cleared. Passing a vector() will cause the local function to be empty.}
  \item{packages}{List of library packages to be loaded by each worker process before computation is started}
  \item{debug}{Runs at debug level}
  \item{trace}{Runs at trace level}
  \item{file.prefix}{Prefix used to create data file}
}
\details{
  Submits work to an LGI project server. \code{lgi.submit} submits the job and returns, while
  \code{lgi.run} waits until the job is finished (or failed).
  The user needs to use \code{lgi.qstat}, \code{lgi.job.status} to monitor the progress
  of jobs; after the job is finished results can be retrieved using \code{lgi.result}.
}
\value{
  LGI job id submitted.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\seealso{
  \code{\link[Rlgi]{lgi.result}}
}

\examples{
\dontrun{
#execute this easy function on the cluster
  jobid <- lgi.submit(function(x) {
                       Sys.sleep(x)
                       x
                    }, 10)
#check the status by passing the job id
  status <- lgi.job.status(lgi.qstat(jobid))
  while(status != 0) {
#continue to check the status, until job is completed
    Sys.sleep(4)
    status <- lgi.job.status(lgi.qstat(jobid))
  }
#once we sure sure that the job is finished, retrieve the results
  result <- lgi.result(jobid)

}
}

\keyword{interface}

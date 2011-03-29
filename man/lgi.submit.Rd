\name{lgi.submit}
\alias{lgi.submit}
\alias{lgi.run}
\title{Performs an asynchronous submission of a function to the remote node}
\description{
  Used to asynchronously submit R tasks to a SGE cluster. lgi.run, makes and synchrinous call to lgi.submit.
}
\usage{
lgi.submit(func, ..., 
           global.savelist=NULL,
           function.savelist=NULL,
           packages=NULL, 
           debug=getOption("lgi.debug"), file.prefix=getOption('lgi.file.prefix')
           )
lgi.run(...)
}
\arguments{
  \item{func}{Function to be executed remotely.}
  \item{...}{Arguments to be passed to the function. For lgi.run, arguments to be passed to lgi.submit}
  \item{global.savelist}{Character vector giving the names of variables from the global environemnt th
at should be imported. If lgi.save.global is set to TRUE, then this will clear the global environment.
 To set the global environment to be empty, use vector()}
  \item{function.savelist}{Character vector giving the variables to save from the local environment.
Passing any arguments here will cause the function environment to be cleared. Passing a vector() will cause the local function to be empty.}
  \item{packages}{List of library packages to be loaded by each worker
    process before computation is started.}
  \item{debug}{Runs at debug level.}
  \item{file.prefix}{Prefix used to create data file}
}
\details{
  Submits work to SGE with an asynchronous qsub call. 
  The user needs to use lgi.job.status and lgi.list.get.result to monitor the progress of jobs and retrieve results.
  
}
\value{
  Returns a list that has the named element filename for the name of the input file created to send commands to the SGE cluster and a named element jobid with the id of the job submitted.
  list(jobid=JOBID, filename=FILENAME)
}
 
\author{Dan Bode \email{dbode@univaud.com}}


\seealso{\code{\link[Rlgi]{lgi.list.get.result}}  }

\examples{

\dontrun{
#execute this easy function on the cluster
  info <- lgi.submit(function(x) {
                       Sys.sleep(x)
                       x
                    }, 10)
#check the status by passing the job id
  status <- lgi.job.status(info$jobid)
  while(status != 0) {
#continue to check the status, until job is completed
    Sys.sleep(4)
    status <- lgi.job.status(info$jobid)
  }
#once we sure sure that the job is finished, retrieve the results
  result <- lgi.list.get.result(info)

# this is a more complicated example that shows how lists can be used to store 
# multiple results

  v1 <- c(1:10)
  func1 <- function(x) {
    Sys.sleep(x)
    x
  }

  l1 <- list(length=length(v1))
  for(i in 1:length(v1)) {
    l1[[i]] <- lgi.submit(func1, v1[[i]])
  }

  r1 <- lapply(l1, lgi.job.status)
  while(! all(r1 == 0)) {
    Sys.sleep(4)
    r1 <- lapply(l1, lgi.job.status)
  }
# its ok to just pass the list of jobid and filename
  x1Par <- lapply(l1, lgi.list.get.result)
}
}
\keyword{interface}

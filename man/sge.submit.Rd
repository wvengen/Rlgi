\name{sge.submit}
\alias{sge.submit}
\alias{sge.run}
\title{Performs an asynchronous submission of a function to the remote node}
\description{
  Used to asynchronously submit R tasks to a SGE cluster. sge.run, makes and synchrinous call to sge.submit.
}
\usage{
sge.submit(func, ..., 
           global.savelist=NULL,
           function.savelist=NULL,
           packages=NULL, 
           debug=getOption("sge.debug"), file.prefix=getOption('sge.file.prefix')
           )
sge.run(...)
}
\arguments{
  \item{func}{Function to be executed remotely.}
  \item{...}{Arguments to be passed to the function. For sge.run, arguments to be passed to sge.submit}
  \item{global.savelist}{Character vector giving the names of variables from the global environemnt th
at should be imported. If sge.save.global is set to TRUE, then this will clear the global environment.
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
  The user needs to use sge.job.status and sge.list.get.result to monitor the progress of jobs and retrieve results.
  
}
\value{
  Returns a list that has the named element filename for the name of the input file created to send commands to the SGE cluster and a named element jobid with the id of the job submitted.
  list(jobid=JOBID, filename=FILENAME)
}
 
\author{Dan Bode \email{dbode@univaud.com}}


\seealso{\code{\link[Rsge]{sge.list.get.result}}  }

\examples{

\dontrun{
#execute this easy function on the cluster
  info <- sge.submit(function(x) {
                       Sys.sleep(x)
                       x
                    }, 10)
#check the status by passing the job id
  status <- sge.job.status(info$jobid)
  while(status != 0) {
#continue to check the status, until job is completed
    Sys.sleep(4)
    status <- sge.job.status(info$jobid)
  }
#once we sure sure that the job is finished, retrieve the results
  result <- sge.list.get.result(info)

# this is a more complicated example that shows how lists can be used to store 
# multiple results

  v1 <- c(1:10)
  func1 <- function(x) {
    Sys.sleep(x)
    x
  }

  l1 <- list(length=length(v1))
  for(i in 1:length(v1)) {
    l1[[i]] <- sge.submit(func1, v1[[i]])
  }

  r1 <- lapply(l1, sge.job.status)
  while(! all(r1 == 0)) {
    Sys.sleep(4)
    r1 <- lapply(l1, sge.job.status)
  }
# its ok to just pass the list of jobid and filename
  x1Par <- lapply(l1, sge.list.get.result)
}
}
\keyword{interface}

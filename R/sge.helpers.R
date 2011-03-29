# $Id: sge.helpers.R,v 1.3 2007/04/01 22:27:22 coultn Exp $

#takes the output from a qsub call, returns the jobid
sge.get.jobid <- function(result)
{
    resultlist  <- unlist(strsplit(result, " "))
    match_index <- grep("\\d+", resultlist, perl = TRUE)
    # we are expecting a number to be the third word returned from qsub on success
    if(3 %in% match_index)
      jobid=as.integer(unlist(strsplit(resultlist[3], "\\."))[1])
    else
      return(NULL)
}
#determines how to split input data
sge.split <- function (x, ncl) {
  # The version in snow of splitrows uses 'F' instead of 'FALSE' and
  # so, causes errors in R CMD check
  if(is.matrix(x) || is.data.frame(x))
    lapply(splitIndices(nrow(x), ncl), function(i) x[i, , drop = FALSE])
  else if(is.list(x) || is.vector(x)) 
    lapply(splitIndices(length(x), ncl), function(i) as.array(x[i]))
  else { 
    warning("Type not allowed to be split.")
    return(NULL)
  }
}

sge.job.status <- function (jobid, qacct=getOption("sge.use.qacct")) {
  system(paste(file.path(.path.package("Rsge"), getOption("sge.monitor.script")), jobid, qacct, getOption("sge.debug"), getOption("sge.qstat")), intern=FALSE)
}

sge.checkNotNow <- function(result) {
  ret <- grep("Your qsub request could not be scheduled, try again later", result, perl=TRUE)
  if(length(ret) == 0) return(FALSE)
  else return(TRUE)
}


# I decided against this, its just too dangerous, people can clean up their 
# files themselves!
#sge.cleanup <- function() {
#  prefix <- getOption("sge.file.prefix")
#  script <- getOption("sge.script")
#  if(nchar(prefix) < 1 || nchar(script) < 1) {
#    warning(paste("invalid file prefix"))
#    return(NULL)
#  }
# be careful if you ever change these line, it could erase an entire dir.
# also this could be bad if the PREFIX is not something unique
#  system(paste("rm -f ", prefix,"*", sep="" ))
#  system(paste("rm -f ", script,".*", sep="" ))
#}

# this function is mainly for testing, it doesnt logically serve a purpose
# because it is rarely a good job to blobk for a single submission
# it will always be slower, the only use case is it the submission machine is loaded.
sge.run <- function(...) {
  info <- sge.submit(...)
  status <- sge.job.status(info$jobid)
  while(status != 0) {
    Sys.sleep(4)
    status <- sge.job.status(info$jobid)
  }
  sge.list.get.result(info)
}

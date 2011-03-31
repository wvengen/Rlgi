# $Id$

#determines how to split input data
lgi.split <- function (x, ncl) {
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

# return XML document containing job information
lgi.qstat <- function(jobid=NULL, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  cmd <- paste(getOption("lgi.qstat"), getOption("lgi.qstat.options"), jobid, getOption("lgi.pipe"));
  lgi.trace('Executing:', cmd, trace=trace)
  result <- paste(system(cmd, intern=TRUE), collapse='')
  lgi.trace('Returns:', result, trace=trace)
  if (length(grep("^\\s*Error", result))>0) {
    warning(result)
    return(NULL)
  } else if (length(grep("^\\s*<", result))==0) {
    warning(paste("Malformed qstat response: ",result))
    return(NULL)
  }
  result <- paste("<root>", result, "</root>")
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}
# return job status string from XML result (qsub/qstat)
lgi.job.status <- function(xml) {
  if (xmlName(xml)=="root") xml <- xml[["job"]]
  return(xmlValue(xml[["state"]]))
}
# return job id from XML result (qsub/qstat)
lgi.job.id <- function(xml) {
  if (xmlName(xml)=="root") xml <- xml[["job"]]
  return(as.integer(xmlValue(xml[["job_id"]])))
}
# return repository url from XML result (qsub/qstat)
lgi.job.repourl <- function(xml) {
  if (xmlName(xml)=="root") xml <- xml[["job"]]
  return(xmlValue(xml[["job_specifics"]][["repository_url"]]))
}

# submit LGI job directly, return XML node containing job information
lgi.qsub <- function(rcode, application, files=c(), debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  qsub          <- getOption("lgi.qsub")
  qsub.user.opt <- getOption("lgi.user.options")
  qsub.options  <- getOption("lgi.qsub.options")
  pipe          <- getOption("lgi.pipe")
  files <- sapply(files, shQuote)
  cmd <- paste(qsub, '-a', shQuote(application), qsub.user.opt, qsub.options, paste(files,collapse=' '), pipe)
  lgi.trace('Executing:', cmd, trace=trace)
  result <- paste(system(cmd, intern=TRUE, input=rcode), collapse='')
  lgi.trace('Returns:', result, trace=trace)
  # parse output
  if (length(grep("^\\s*(Error|Cannot)", result))>0) {
    stop(result)
    return()
  } else if (length(grep("^\\s*<", result))==0) {
    stop("Malformed qsub response: ",result)
    return()
  }
  result <- paste("<root>", result, "</root>")
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}

# low-level LGI filetransfer utility
lgi.filetransfer <- function(action, repo, files, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  ft          <- getOption("lgi.filetransfer")
  ft.options  <- getOption("lgi.filetransfer.options")
  pipe          <- getOption("lgi.pipe")
  # broken LGI_filetransfer doesn't accept -x on download :(
  if (action=="download" || action=="upload")
    ft.options = gsub('(^|\\s)-x(\\s|$)', '\\1', ft.options)
  files <- sapply(files, shQuote)
  cmd <- paste(ft, ft.options, action, shQuote(repo), paste(files, collapse=' '), pipe)
  lgi.trace('Executing:', cmd, trace=trace)
  result <- paste(system(cmd, intern=TRUE), collapse='')
  lgi.trace('Returns:', result, trace=trace)
  # parse output
  if (length(grep("^\\s*Error", result))>0) {
    stop(result)
    return()
  } else if (action=="download" || action=="upload") {
    # no XML result for download or upload :(
    return()
  } else if (length(grep("^\\s*<", result))==0) {
    stop("Malformed filetransfer response: ",result)
    return()
  }
  result <- paste("<root>", result, "</root>")
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}
# retrieve files from repository
lgi.file.get <- function(repo, files) {
  result = lgi.filetransfer("download", repo, files)
}
# upload files to repository
lgi.file.put <- function(repo, files) {
  result = lgi.filetransfer("upload", repo, files)
}


# this function is mainly for testing, it doesnt logically serve a purpose
# because it is rarely a good job to block for a single submission
# it will always be slower, the only use case is if the submission machine is loaded.
lgi.run <- function(..., debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  jobid <- lgi.submit(..., debug=debug, trace=trace)
  status <- lgi.job.status(lgi.qstat(jobid, debug=debug, trace=trace))
  while(status %in% c("queued", "scheduled", "running")) {
    Sys.sleep(4)
    status <- lgi.job.status(lgi.qstat(jobid, debug=debug, trace=trace))
    cat("Job status:", status, "\r")
  }
  cat("\n")
  if (status!="finished") stop("Job did not finish succesfully")
  return(lgi.result(jobid, debug=debug, trace=trace))
}

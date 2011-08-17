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

# POSTs an HTTPS request to the LGI project server
lgi.request <- function(apipath, variables=c(), files=c(), path=NA, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  # TODO create connection if not available so we can reuse it
  mimeboundary = '@$_Th1s_1s_th3_b0und@ry_@$'
  data <- paste(c(
      sapply(names(variables), function(name) c(
        paste('--', mimeboundary,  sep=''),
        paste('Content-Disposition: form-data; name="', name, '"',  sep=''),
	'',
	variables[[name]]
      )),
      sapply(names(files), function(name) c(
        paste('--', mimeboundary,  sep=''),
        paste('Content-Disposition: form-data; name="', name, '"; filename="', files[[name]][[1]], '"',  sep=''),
	'',
	files[[name]][[2]]
      )),
      paste('--', mimeboundary, '--',  sep=''),
      ''
    ), collapse='\r\n')
  headers <- c(
    'Content-Type' = paste('multipart/form-data; boundary=', mimeboundary,  sep=''),
    'Accept' = 'text/plain',
    'Connection' = 'keep-alive'
  )
  if (is.na(path)) path <- paste(getOption("lgi.server"), '/', apipath,  sep='')
  return(getURL(path, postfields=data, httpheader=headers,
    customrequest='POST',
    cainfo=getOption("lgi.cacert"),
    sslcert=getOption("lgi.certificate"),
    sslkey=getOption("lgi.privatekey")))
}

# return XML document containing job information
lgi.qstat <- function(jobid=NULL, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  args <- c(
    'project' = getOption('lgi.project'),
    'user' = getOption('lgi.user'),
    'groups' = getOption('lgi.groups'),
    'job_id' = jobid
  )
  result <- lgi.request('/interfaces/interface_job_state.php', args)
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  result <- result[["response"]]
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}
# return job status string from XML result (qsub/qstat)
lgi.job.status <- function(xml) {
  if (xmlName(xml)=="response") xml <- xml[["job"]]
  return(xmlValue(xml[["state"]]))
}
# return job id from XML result (qsub/qstat)
lgi.job.id <- function(xml) {
  if (xmlName(xml)=="response") xml <- xml[["job"]]
  return(as.integer(xmlValue(xml[["job_id"]])))
}
# return repository url from XML result (qsub/qstat)
lgi.job.repourl <- function(xml) {
  if (xmlName(xml)=="response") xml <- xml[["job"]]
  return(xmlValue(xml[["job_specifics"]][["repository_url"]]))
}

# submit LGI job directly, return XML node containing job information
lgi.qsub <- function(rcode, application, files=c(), targetResources='any', writeAccess=NA, readAccess=NA, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  args <- na.omit(c(
    'project' = getOption('lgi.project'),
    'user' = getOption('lgi.user'),
    'groups' = getOption('lgi.groups'),
    'application' = application,
    'target_resources' = targetResources,
    'write_access' = writeAccess,
    'read_access' = readAccess,
    'input' = lgi.binhex(rcode),
    'number_of_uploaded_files' = length(files)
  ))
  # read files (must all fit in memory!)
  filesc = c()
  for (fname in files) {
    fdata = readBin(fname, what='raw', n=file.info(fname)$size)
    filesc[[paste('uploaded_file_', length(filesc)+1, sep='')]] = c(fname, fdata)
  }
  result <- lgi.request('/interfaces/interface_submit_job.php', args, filesc)
  # parse output
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  result <- result[["response"]]
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}

# low-level LGI filetransfer utility
lgi.filetransfer <- function(action, repo, files, debug=getOption("lgi.debug"), trace=getOption("lgi.trace")) {
  # TODO use curl to transfer

  # parse output
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  result <- result[["response"]]
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

lgi.binhex <- function(b) {
  # there must be a better way ... please let me know!
  return(paste(sapply(unlist(strsplit(b,'')), function(x){sprintf('%02x',as.integer(charToRaw(x)))}), collapse=''))
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


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

# create a new Curl handle or return an existing one
lgi.curl.getHandle <- function() {
  if (!exists('lgi.curl.handle')) {
    assign('lgi.curl.handle', getCurlHandle(
      cainfo=getOption("lgi.cacert"),
      sslcert=getOption("lgi.certificate"),
      sslkey=getOption("lgi.privatekey")
    ), envir=.GlobalEnv)
  }
  return(lgi.curl.handle)
}

# POSTs an HTTPS request to the LGI project server
lgi.request <- function(apipath, variables=c(), files=c(), path=NA, trace=getOption("lgi.trace")) {
  data <- as.list(variables)
  if (length(files)>0) {
    for (i in 1:length(files)) {
      data[[paste('uploaded_file_',i,sep='')]] = fileUpload(files[i])
    }
  }
  headers <- c(
    'Accept' = 'text/plain',
    'Connection' = 'keep-alive'
  )
  if (is.na(path)) path <- paste(getOption("lgi.server"), '/', apipath,  sep='')
  return(postForm(path, .params=data, style='httppost', curl=lgi.curl.getHandle(), .opts=list(
    verbose=as.logical(trace)
  )))
}

# return XML document containing job information
lgi.qstat <- function(jobid=NULL, trace=getOption("lgi.trace")) {
  args <- c(
    'project' = getOption('lgi.project'),
    'user' = getOption('lgi.user'),
    'groups' = getOption('lgi.groups'),
    'job_id' = jobid
  )
  result <- lgi.request('/interfaces/interface_job_state.php', args, trace=trace)
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
lgi.qsub <- function(rcode, application, files=c(), targetResources='any', writeAccess=NA, readAccess=NA, trace=getOption("lgi.trace")) {
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
  result <- lgi.request('/interfaces/interface_submit_job.php', args, files, trace=trace)
  # parse output
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  result <- result[["response"]]
  if (!is.null(result[["error"]])) stop(xmlValue(result[["error"]][["message"]]))
  return(result)
}

# retrieve filenames from repository to current directory (reads full file into memory)
lgi.file.get <- function(repo, files, trace=getOption('lgi.trace')) {
  for (fn in files) {
    # I've tried using curlPerform with file=CFILE(fn, mode='wb')@ref
    # but the last chunck would not be written until R exit and I've
    # found no way to close the CFILE.
    content <- getBinaryURL(paste(repo, fn,  sep='/'),
      curl=lgi.curl.getHandle(),
      verbose=as.logical(trace)
    )
    f = file(fn, 'wb')
    writeBin(content, f)
    close(f)
  }
}

# upload files to repository
lgi.file.put <- function(repo, files, trace=getOption('lgi.trace')) {
  for (fn in files) {
    result <- curlPerform(url=paste(repo, basename(fn),  sep='/'),
      upload=TRUE,
      readdata=CFILE(fn, mode='rb')@ref,
      infilesize=file.info(fn)$size,
      curl=lgi.curl.getHandle(),
      verbose=as.logical(trace)
    )
  }
}

# list files in repository
lgi.file.list <- function(repo, trace=getOption('lgi.trace')) {
  # parse components of url
  server <- sub('[^/]+$', '', repo)
  dir <- sub('^.*/', '', repo)
  # do request
  result <- getURL(paste(server, '../repository_content.php?repository=', dir, sep=''),
    curl=lgi.curl.getHandle(),
    verbose=as.logical(trace)
  )
  # return result
  result <- xmlRoot(xmlTreeParse(result, asText=TRUE))
  files <- c()
  for (r in xmlChildren(result)) {
    files[[ xmlAttrs(r)[['name']] ]] = xmlApply(r, xmlValue)
  }
  return(files)
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


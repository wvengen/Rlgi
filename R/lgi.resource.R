# $Id$

# detect whether running as a resource or no
lgi.resource.check <- function(dir='.') {
  serverfile = paste(dir, '/', 'LGI_this_project_server', sep='')
  if (!file.exists(serverfile)) return(FALSE)
  hashfile = paste(serverfile, sep='')
  if (!file.exists(hashfile)) return(FALSE)
  return(TRUE)
}

# read settings when running as a resource
lgi.resource.readConfig <- function(dir='.') {
   optmapping = c(
    lgi.server='LGI_this_project_server',
    lgi.project='LGI_project',
    lgi.application='LGI_application',
    lgi.user='LGI_owners',
    lgi.groups='LGI_read_access',
    lgi.cacert='LGI_ca_certificate_file',
    lgi.certificate='LGI_certificate_file',
    lgi.privatekey='LGI_key_file'
  )
 options(as.list(na.omit(sapply(optmapping, function(x) {
    f = paste(dir, '/', x, sep='');
    return(scan(f, '', quiet=TRUE))
  }))))
  return(TRUE)
}

# return a resource job's repository url
#  TODO make this more general,
lgi.resource.repourl <- function(dir='.') {
  con <- file(paste(dir, '/', 'LGI_job_specifics', sep=''), 'r')
  job_specifics <- readLines(con, warn=FALSE)
  close(con)
  job_specifics <- paste('<job_specifics>', job_specifics, '</job_specifics>')
  job_specifics <- xmlRoot(xmlTreeParse(job_specifics, asText=TRUE))
  return(xmlValue(job_specifics[["repository_url"]]))
}


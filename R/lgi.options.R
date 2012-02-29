# $Id$

#
# This sets all of the global options
#
#
lgi.setDefaultOptions <- function() {
  # user variables, feel free to change for more info help(lgi.options)
  #LGI server to contact
  options(lgi.server='https://example.com/LGI/')
  #project to use on server
  options(lgi.project='helloworld')
  #default application to submit to
  options(lgi.application='R') # TODO use NULL as default; add checking somewhere
  #certificate authority to check LGI server against
  options(lgi.cacert=NULL)
  #certificate for authentication with LGI server
  options(lgi.certificate=NULL)
  #key for authentication with LGI server
  options(lgi.privatekey=NULL)
  #LGI user (this could have been autodetected if there were openssl on CRAN)
  options(lgi.user=NA)
  #LGI groups
  options(lgi.groups=NA)
  #if the cluster should be used or if it should be run locally.
  options(lgi.use.cluster=TRUE)
  # default number of elements per split
  options(lgi.block.size=100)
  #prefix for data files
  options(lgi.file.prefix="Rlgi_data")
  #should the files be removed afterwards (data files and LGI output files)
  options(lgi.remove.files=TRUE)
  #logging levels
  options(lgi.debug=TRUE)
  options(lgi.trace=FALSE)
  # should we save the global environment by default.
  options(lgi.save.global=FALSE)

  # now read user configuration
  lgi.readConfig()
}

lgi.options <- function(...) {
  options(...)
}
lgi.getOption <- function(...) {
  getOption(...)
}
# return default configuration file or directory
#   see https://github.com/wvengen/LGI/wiki/User-configuration
lgi.getDefaultConfig <- function() {
  # environment variable gets preference
  filename = Sys.getenv('LGI_CONFIG', NA)
  if (!is.na(filename))
     return(filename)
  # or else try default files
  filename = path.expand('~/LGI.cfg')
  if (!file.exists(filename))
    filename = path.expand('~/.LGI/')
  if (!file.exists(filename))
    filename = path.expand('~/.LGI/LGI.cfg')
  if (!file.exists(filename))
    return(NA)
  return(filename)
}
# read settings from single configuration file or directory
lgi.readConfig <- function(filename=NA) {
  # figure out default file to read when none given
  if (is.na(filename)) {
    filename = lgi.getDefaultConfig()
    if (is.na(filename) || !file.exists(filename)) {
      warning("Could not read default LGI configuration from ~/LGI.cfg, ~/LGI or LGI_CONFIG.")
      return(FALSE)
    }
  }
  # fields to read from configuration
  optmapping = c(
    lgi.server='defaultserver',
    lgi.project='defaultproject',
    lgi.application='defaultapplication',
    lgi.user='user',
    lgi.groups='groups'
  )
  filemapping = c(
    lgi.cacert='ca_chain',
    lgi.certificate='certificate',
    lgi.privatekey='privatekey'
  )
  if (file.info(filename)[['isdir']]) {
    # read from configuration directory
    options(as.list(na.omit(sapply(optmapping, function(x) {
        x=paste(filename, x, sep='');
        if (file.exists(x)) scan(x, '', quiet=TRUE) else NA
      }))))
    options(as.list(na.omit(sapply(filemapping, function(x) {
        x=paste(filename, x, sep='');
        if (file.exists(x)) x else NA
      }))))
  } else {
    # read from configuration xml file
    cfg = xmlRoot(xmlTreeParse(filename))
    if (xmlName(cfg)!='LGI_user_config')
      stop('Malformed LGI user configuration file (root element)')
    options(as.list(na.omit(sapply(optmapping, function(x) {
        xmlValue(cfg[[x]])
      }))))
    # when certificate/key options exist, use file as variable
    options(as.list(na.omit(sapply(filemapping, function(x) {
        if (!is.na(xmlValue(cfg[[x]]))) filename
      }))))
  }
  return(TRUE)
}

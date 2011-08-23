# $Id$

#
# This sets all of the global options
#
#
lgi.setDefaultOptions <- function() {
  # user variables, feel free to change for more info help(lgi.options)
  #LGI server to contact
  options(lgi.server=lgi.getLGIConfig('defaultserver', 'https://example.com/LGI/'))
  #project to use on server
  options(lgi.project=lgi.getLGIConfig('defaultproject', 'helloworld'))
  #default application to submit to
  options(lgi.application="R") # TODO use NULL as default; add checking somewhere
  #certificate authority to check LGI server against
  options(lgi.cacert=path.expand('~/.LGI/ca_chain'))
  #certificate for authentication with LGI server
  options(lgi.certificate=path.expand('~/.LGI/certificate'))
  #key for authentication with LGI server
  options(lgi.privatekey=path.expand('~/.LGI/privatekey'))
  #LGI user (this could have been autodetected if there were openssl on CRAN)
  options(lgi.user=lgi.getLGIConfig('user', NA))
  #LGI groups
  options(lgi.groups=lgi.getLGIConfig('groups', NA))
  #if the cluster should be used or if it should be run locally.
  options(lgi.use.cluster="TRUE")
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

  # now read single-file configuration for easy setup by one file
  lgi.readConfig()
}

lgi.options <- function(...) {
  options(...)
}
lgi.getOption <- function(...) {
  getOption(...)
}
# read configuration file in ~/.LGI, or use default if file doesn't exist
lgi.getLGIConfig <- function(name, default) {
  filename <- path.expand(paste('~/.LGI/', name, sep=''))
  if (!file.exists(filename)) return (default)
  return(scan(filename, '', quiet=TRUE))
}
# read settings from single configuration file
lgi.readConfig <- function(filename=path.expand('~/LGI.cfg')) {
  if (!file.exists(filename)) return(FALSE)
  mapping = c( # fields to read from configuration
    lgi.server='defaultserver',
    lgi.project='defaultproject',
    lgi.application='defaultapplication',
    lgi.user='user',
    lgi.groups='groups'
  )
  cfg = xmlRoot(xmlTreeParse(filename))
  if (xmlName(cfg)!='LGI_user_config')
    stop('Malformed LGI user configuration file (root element)')
  # set variables
  options(as.list(na.omit(sapply(mapping, function(x){ xmlValue(cfg[[x]]) }))))
  # when certificate/key options exist, use file as variable
  if (!is.na(xmlValue(cfg[['ca_chain']])))
    options(lgi.cacert=filename)
  if (!is.na(xmlValue(cfg[['certificate']])))
    options(lgi.certificate=filename)
  if (!is.na(xmlValue(cfg[['privatekey']])))
    options(lgi.privatekey=filename)
  return(TRUE)
}

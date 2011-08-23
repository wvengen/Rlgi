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
}

lgi.options <- function(...) {
  options(...)
}
lgi.getOption <- function(...) {
  getOption(...)
}
# read LGI configuration file, or use default if file doesn't exist
lgi.getLGIConfig <- function(name, default) {
  filename <- path.expand(paste('~/.LGI/', name, sep=''))
  if (!file.exists(filename)) return (default)
  return(scan(filename, '', quiet=TRUE))
}

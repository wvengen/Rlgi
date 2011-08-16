# $Id$

#
# This sets all of the global options
#
#
lgi.setDefaultOptions <- function() {
  # user variables, feel free to change for more info help(lgi.options)
  #default LGI server to contact
  options(lgi.server="https://example.com/LGI")
  # default project to use on server
  options(lgi.project="helloworld")
  #default application to submit to
  options(lgi.application="R") # TODO use NULL as default; add checking somewhere
  # default certificate authority to check LGI server against
  options(lgi.cacert=path.expand('~/.LGI/ca_chain'))
  # default certificate for authentication with LGI server
  options(lgi.usercert=path.expand('~/.LGI/certificate'))
  # default key for authentication with LGI server
  options(lgi.userkey=path.expand('~/.LGI/privatekey'))
  #if the cluster should be used or if it should be run locally.
  options(lgi.use.cluster="TRUE")
  # default number of elements per split
  options(lgi.block.size=100)
  #prefix for data files
  options(lgi.file.prefix="Rlgi_data")
  #should the files be removed afterwards (data files and LGI output files)
  options(lgi.remove.files=TRUE)
  #logging levels, this could be replaced by something more useful
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

# $Id$

.First.lib <-  function(libname, pkgname) {
  require(snow)
  require(RCurl)
  require(XML)
  lgi.setDefaultOptions()
  cat(paste("\nWelcome to Rlgi\n"))
  cat(paste("    Version:", packageDescription(pkgname, field = "Version"), "\n\n"))
}


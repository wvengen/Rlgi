# $Id$

.First.lib <-  function(libname, pkgname) {
  require(stats) # for na.omit
  require(snow)
  require(RCurl)
  require(XML)
  lgi.setDefaultOptions()
}



#
# This sets all of my global options
#
#
lgi.setDefaultOptions <- function() {
  # user variables, feel free to change for more info help(lgi.options)
  #if the cluster should be used or if it should be run locally.
  options(lgi.use.cluster="TRUE")
  # default number of elements per split
  options(lgi.block.size=100)
  #prefix for data files
  options(lgi.file.prefix="Rlgi_data")
  #path for qsub
  options(lgi.qsub="LGI_qsub")
  #path for qstat
  options(lgi.qstat="LGI_qstat")
  #path for filetransfer
  options(lgi.filetransfer="LGI_filetransfer")
  #default application to submit to
  options(lgi.application='R-2.12') # TODO use NULL as default; add checking somewhere
  #user options for lgi
  options(lgi.user.options="")
  #should the files be removed afterwards (data files and LGI output files)
  options(lgi.remove.files=TRUE)
  #logging levels, this will be replaced by something useful soon
  options(lgi.debug=TRUE)
  options(lgi.trace=FALSE)
  # should we save the global environment by default.
  options(lgi.save.global=FALSE)
  # global variables that should not be changed, or should be changed very carefully. 
  options(lgi.qsub.options="-x -i /dev/stdin")
  options(lgi.qstat.options="-x")
  options(lgi.filetransfer.options="-x")
  #options(lgi.pipe="| fold -w 4000") # avoid R warning "may be truncated in call to system"; may break stuff
}

lgi.options <- function(...) {
  options(...)
}
lgi.getOption <- function(...) {
  getOption(...)
}

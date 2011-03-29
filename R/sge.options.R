
#
# This sets all of my global options
#
#
sge.setDefaultOptions <- function() {
  # user variables, feel free to change for more info help(sge.options)
  #if the cluster should be used or if it should be run locally.
  options(sge.use.cluster="TRUE")
  # default number of elements per split
  options(sge.block.size=100)
  #prefix for data files
  options(sge.file.prefix="Rsge_data")
  #path for qsub
  options(sge.qsub="qsub")
  #path for qstat
  options(sge.qstat="qstat")
  #path for qacct
  options(sge.qacct="qacct")
  #user options for sge
  options(sge.user.options="-S /bin/bash")
  #extension for result files
  options(sge.ret.ext="sge.ret")
  #should qacct be used to help determine job status
  options(sge.use.qacct=FALSE)
  #should the files be removed afterwards (data files and sge output files)
  options(sge.remove.files=TRUE)
  #logging levels, this will be replaced by something useful soon
  options(sge.debug=FALSE)
  options(sge.trace=TRUE)
  # should we save the global environment by default.
  options(sge.save.global=FALSE)
  # global variables that should not be changed, or should be changed very carefully. 
  options(sge.qsub.options="-cwd")
  options(sge.qsub.blocking="-sync y -t 1-")
  options(sge.script="RunSgeJob")
  options(sge.monitor.script="MonitorJob.sh")
}

sge.options <- function(...) {
  options(...)
}
sge.getOption <- function(...) {
  getOption(...)
}

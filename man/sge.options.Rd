\name{sge.options}
\alias{sge.options}
\alias{sge.getOption}
\alias{sge.setDefaultOptions}
\title{Allows a user to examine and reset values of global parameters}
\description{
  Allows a user to examine and reset values of global parameters
}
\usage{
sge.options(...)
sge.getOption(...)
sge.setDefaultOptions()
}
\arguments{
  \item{...}{Either empty, or a succession of parameter names in quotes, or a succession of name=value pairs. See below for the parameter names.}
}
\details{
 This function allows the user to examine and reset the values of global parameters. It is analogous to the system function options. 

set.setDefaultOptions is used to set all of the default options for sge, it is mainly used by the loader to init options.

 The global parameters are:

   sge.save.global
     Should the global environment be saved by default? Default is TRUE (this may change in future versions)
   sge.use.cluster
     Specifies if the run should be performed using an SGE cluster. Default is TRUE.
   sge.block.size
     Specifies the size of the blocks of X used if njobs is not specified. Default is 100.
   sge.file.prefix
     Prefix to be used for data and return files. Default is Rsge\_data.
   sge.qsub
     qsub to call. Default assumes that qsub is in the path. Change this if it is not.
   sge.qstat
     qstat to call. Default assumes that qstat is in the path. Change this if it is not. 
   sge.qacct
     qacct to call. Default assumes that qacct is in the path. Change this if it is not.
   sge.user.options
     user options to be passed to qsub, change this if you want to. Default is -S /bin/bash. If this value     is changed, remember that the scheduler must still run bash. 
   sge.ret.ext
     extension used for the return files. Default sge.ret
   sge.remove.files
     Specifies if files should be removed after execution. This applies to both data files as well as qsub output files. Default is TRUE. 
   sge.use.qacct
     Determines if the qacct command is configured for usage. Default is FALSE.
   sge.debug
     If set to TRUE, then the program will run on debug mode. Default is FALSE.
   sge.trace
     If set to TRUE, some status information will be showed. Default is TRUE. 

  The following were included as parameters for the developers, but not currently supported. It is recommeded that they are not changed unless the user had read the code and understands the implications of the changes.

   sge.qsub.options
     options to be passed to qsub. Default is -cwd. There are some
     operations that require this to be -cwd.
   sge.qsub.blocking
     options passed to qsub to indicate a blocking call. CANNOT BE CHANGED. Default -sync y -t 1-. 
   sge.script
     location of the sge.script file. Default RunSgeJob
   sge.monitor.script
     location of the monitor file. Default MonitorJob.sh.
}
\value{
  no return.
}
 
\author{Dan Bode \email{dbode@univaud.com}}

\keyword{interface}

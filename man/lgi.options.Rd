\name{lgi.options}
\alias{lgi.options}
\alias{lgi.getOption}
\alias{lgi.setDefaultOptions}
\title{Allows a user to examine and reset values of global parameters}
\description{
  Allows a user to examine and reset values of global parameters
}
\usage{
lgi.options(...)
lgi.getOption(...)
lgi.setDefaultOptions()
}
\arguments{
  \item{...}{Either empty, or a succession of parameter names in quotes, or a succession of name=value pairs. See below for the parameter names.}
}
\details{
 This function allows the user to examine and reset the values of global parameters. It is analogous to the system function options. 

set.setDefaultOptions is used to set all of the default options for sge, it is mainly used by the loader to init options.

 The global parameters are:

   lgi.save.global
     Should the global environment be saved by default? Default is TRUE (this may change in future versions)
   lgi.use.cluster
     Specifies if the run should be performed using an SGE cluster. Default is TRUE.
   lgi.block.size
     Specifies the size of the blocks of X used if njobs is not specified. Default is 100.
   lgi.file.prefix
     Prefix to be used for data and return files. Default is Rlgi\_data.
   lgi.qsub
     qsub to call. Default assumes that LGI_qsub is in the path. Change this if it is not.
   lgi.qstat
     qstat to call. Default assumes that LGI_qstat is in the path. Change this if it is not. 
   lgi.user.options
     user options to be passed to qsub, change this if you want to. Default is -S /bin/bash. If this value     is changed, remember that the scheduler must still run bash. 
   lgi.ret.ext
     extension used for the return files. Default lgi.ret
   lgi.remove.files
     Specifies if files should be removed after execution. This applies to both data files as well as qsub output files. Default is TRUE. 
   lgi.debug
     If set to TRUE, then the program will run on debug mode. Default is FALSE.
   lgi.trace
     If set to TRUE, some status information will be showed. Default is TRUE. 

  The following were included as parameters for the developers, but not currently supported. It is recommeded that they are not changed unless the user had read the code and understands the implications of the changes.

   lgi.qsub.options
     options to be passed to qsub. Default is -cwd. There are some
     operations that require this to be -cwd.
   lgi.qsub.blocking
     options passed to qsub to indicate a blocking call. CANNOT BE CHANGED. Default -sync y -t 1-. 
   lgi.script
     location of the lgi.script file. Default RunLgiJob
   lgi.monitor.script
     location of the monitor file. Default MonitorLgiJob.sh.
}
\value{
  no return.
}
 
\author{Dan Bode \email{dbode@univaud.com}}

\keyword{interface}

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

\code{set.setDefaultOptions} is used to set all of the default options for LGI, it is mainly used by the loader to init options.

 The global parameters are:

   \code{lgi.save.global}
     Should the global environment be saved by default? Default is TRUE (this may change in future versions)
   \code{lgi.use.cluster}
     Specifies if the run should be performed using an LGI cluster. Default is TRUE.
   \code{lgi.block.size}
     Specifies the size of the blocks of X used if njobs is not specified. Default is 100.
   \code{lgi.file.prefix}
     Prefix to be used for data and return files. Default is \code{Rlgi\_data}.
   \code{lgi.application}
     LGI application to submit to. Default is "R", though this may differ in your setup.
   \code{lgi.qsub}
     qsub program to call. Default assumes that LGI_qsub is in the path. Change this if it is not.
   \code{lgi.qstat}
     qstat program to call. Default assumes that LGI_qstat is in the path. Change this if it is not. 
   \code{lgi.filetransfer}
     filetransfer program to call. Default assumes that LGI_filetransfer is in the path. Change this if it is not. 
   \code{lgi.user.options}
     user options to be passed to qsub, change this if you want to e.g. to modify the default username or LGI configuration directory.
   \code{lgi.remove.files}
     Specifies if generated files should be removed after execution. Default is TRUE. 
   \code{lgi.debug}
     If set to TRUE, then the program will run on debug mode. Default is FALSE.
   \code{lgi.trace}
     If set to TRUE, some status information will be showed. Default is TRUE. 

  The following were included as parameters for the developers, but not currently supported. It is recommended that they are not changed unless the user had read the code and understands the implications of the changes.

   \code{lgi.qsub.options}
     options to be passed to qsub. Default is "-x -i /dev/stdin", which are mandatory.
   \code{lgi.qstat.options}
     options to be passed to qstat. Default is "-x", which is mandatory.
   \code{lgi.filetransfer}
     options to be passed to filetransfer. Default is "-x", which is mandatory.
   \code{lgi.pipe}
     command to paste after qsub, qstat and filetransfer commands, e.g. to filter using pipes. Default is none.
}
\value{
  no return.
}
 
\author{Dan Bode \email{dbode@univaud.com}}
\author{W. van Engen \email{wvengen@nikhef.nl}}

\keyword{interface}

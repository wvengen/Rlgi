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

   \code{lgi.server}
     LGI project server to contact. Default is read from \code{~/.LGI/defaultserver}.
   \code{lgi.project}
     LGI project to use on server. Default is read from \code{~/.LGI/defaultproject}.
   \code{lgi.application}
     LGI application to submit to. Default is \code{R}.
   \code{lgi.cacert}
     LGI CA certificate to verify server (PEM). Default is \code{~/.LGI/ca_chain}.
   \code{lgi.certificate}
     LGI user certificate to authenticate with (PEM). Default is \code{~/.LGI/certificate}.
   \code{lgi.privatekey}
     LGI user key to authenticate with (PEM). Default is \code{~/.LGI/privatekey}.
   \code{lgi.user}
     LGI username. Default is read from \code{~/.LGI/user}.
   \code{lgi.groups}
     LGI groups to submit jobs with. Default is read from \code{~/.LGI/groups}.
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
   \code{lgi.remove.files}
     Specifies if generated files should be removed after execution. Default is TRUE. 
   \code{lgi.debug}
     If set to TRUE, then the program will run on debug mode. Default is FALSE.
   \code{lgi.trace}
     If set to TRUE, some status information will be showed. Default is TRUE. 
}
\value{
  no return.
}
 
\author{Dan Bode \email{dbode@univaud.com}}
\author{W. van Engen \email{wvengen@nikhef.nl}}

\keyword{interface}

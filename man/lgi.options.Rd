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
lgi.readConfig(filename=path.expand('~/LGI.cfg'))
}
\arguments{
  \item{...}{Either empty, or a succession of parameter names in quotes, or a succession of name=value pairs. See below for the parameter names.}
  \item{filename}{Filename containing XML configuration for LGI connection details.}
}
\details{
 This function allows the user to examine and reset the values of global parameters. It is analogous to the system function options. 

\code{set.setDefaultOptions} is used to set all of the default options for LGI, it is mainly used by the loader to init options.

 The global parameters are:

 \itemize{
   \item{\code{lgi.server}}{LGI project server to contact. Default is read from \code{defaultserver} in \code{~/LGI.cfg} or \code{~/.LGI/defaultserver}.}
   \item{\code{lgi.project}}{LGI project to use on server. Default is read from \code{defaultproject} in \code{~/LGI.cfg} or \code{~/.LGI/defaultproject}.}
   \item{\code{lgi.application}}{LGI application to submit to. Default is read from \code{defaultapplication} in \code{~/LGI.cfg} or else set to \code{R}.}
   \item{\code{lgi.cacert}}{LGI CA certificate to verify server (PEM). Default is \code{ca_chain} in \code{~/LGI.cfg} or \code{~/.LGI/ca_chain}.}
   \item{\code{lgi.certificate}}{LGI user certificate to authenticate with (PEM). Default is \code{certificate} in \code{~/LGI.cfg} or \code{~/.LGI/certificate}.}
   \item{\code{lgi.privatekey}}{LGI user key to authenticate with (PEM). Default is \code{privatekey} in \code{~/LGI.cfg} or \code{~/.LGI/privatekey}.}
   \item{\code{lgi.user}}{LGI username. Default is read from \code{user} in \code{~/LGI.cfg} or \code{~/.LGI/user}.}
   \item{\code{lgi.groups}}{LGI groups to submit jobs with. Default is read from \code{groups} in \code{~/LGI.cfg} or \code{~/.LGI/groups}.}
   \item{\code{lgi.save.global}}{Should the global environment be saved by default? Default is TRUE (this may change in future versions)}
   \item{\code{lgi.use.cluster}}{Specifies if the run should be performed using an LGI cluster. Default is TRUE.}
   \item{\code{lgi.block.size}}{Specifies the size of the blocks of X used if njobs is not specified. Default is 100.}
   \item{\code{lgi.file.prefix}}{Prefix to be used for data and return files. Default is \code{Rlgi\_data}.}
   \item{\code{lgi.application}}{LGI application to submit to. Default is "R", though this may differ in your setup.}
   \item{\code{lgi.remove.files}}{Specifies if generated files should be removed after execution. Default is TRUE.}
   \item{\code{lgi.debug}}{If set to TRUE, then the program will run on debug mode. Default is FALSE.}
   \item{\code{lgi.trace}}{If set to TRUE, some status information will be showed. Default is TRUE.}
  }

 The LGI client tools (\code{LGI\_qstat}, \code{LGI\_qsub} and \code{LGI\_filetransfer} (part of LGI) use configuration files in \code{~/.LGI}. These are recognised by this package as well. To make configuration easier for the user, a single-file configuration method was added: when \code{~/LGI.cfg} is present, options are set from this XML file (overriding any \code{~/.LGI} options). An example would be the following:
\preformatted{
<LGI_user_config>
  <defaultserver>https://www.example.com/LGI/</defaultserver>
  <defaultproject>helloworld</defaultproject>
  <defaultapplication>R-2.12</defaultapplication>
  <user>john</user>
  <groups>john, R-users</groups>
  <certificate>
-----BEGIN CERTIFICATE-----
(...)
-----END CERTIFICATE-----
  </certificate>
  <privatekey>
-----BEGIN RSA PRIVATE KEY-----
(...)
-----END RSA PRIVATE KEY-----
  </privatekey>
  <ca_chain>
-----BEGIN CERTIFICATE-----
(...)
-----END CERTIFICATE-----
  </ca_chain>
</LGI_user_config>
}
Note that the entry \code{certificate} must come before \code{ca_chain}, and that the certificate and key sections (PEM blobs) must not be indented.
}
\value{
  no return.
}
 
\author{W. van Engen \email{wvengen@nikhef.nl}}

\keyword{interface}

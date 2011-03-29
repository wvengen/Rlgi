# $Id: sge.submit.R,v 1.3 2007/04/01 22:27:22 coultn Exp $

"sge.submit" <- function(func, ..., 
                         global.savelist=NULL, 
                         function.savelist=NULL, 
                         packages=NULL,
                         debug=getOption('sge.debug'),
                         file.prefix=getOption('sge.file.prefix')
                         )
  # savelist is a character vector of *names* of objects to be
  # copied to the remote R session
  {
   
    fname <- tempfile(pattern = file.prefix, tmpdir = getwd())
    sge.globalPrep(func, ..., 
                   global.savelist=global.savelist,
                   function.savelist=function.savelist,
                   sge.packages=packages,
                   debug=debug,prefix=fname
                  )
    #sge.call <- as.call(list(func, ...) )
    #sge.packages <- packages
    #savelist <- c(savelist, "sge.call", "sge.packages")
    #save(list=savelist, file=fname)

    qsub          <- getOption("sge.qsub")
    qsub.user.opt <- getOption("sge.user.options")
    qsub.options  <- getOption("sge.qsub.options")
    qsub.script   <- getOption("sge.script")
    script <- paste(file.path(.path.package("Rsge"), qsub.script), fname)
    result <- system(paste(qsub, qsub.user.opt, qsub.options, script), intern = TRUE)
    if(debug) print(result)
    sge.fname <- paste(fname,".", getOption("sge.ret.ext"), sep="")
    list(jobid=sge.get.jobid(result), filename=sge.fname)
  }

# $Id$
#
# This is the main script for each computing node. It restores the environment
# and saved by the Rlgi package and runs the user function.
# It is assumed that the following variables are set in the environment prior
# to running this script:
#   lgi.x.globalenv     string with filename of global environment data
#   lgi.x.functionenv   string with filename of function environment data
#   lgi.x.result        string with filename to write result to
#   lgi.x.par           boolean indicating if this is a parApply invocation or no
#   lgi.x.parX          apply value for current parApply invocation
# So one could use the following piece of R code on a computing node (after
# having made sure this file is present there):
#   lgi.x.PAR <- FALSE
#   source('runjob.R', local=TRUE)

lgi.ret <- try({
  #lgi.x.globalenv = paste(lgi.file.prefix, "-GLOBAL", sep="")
  #lgi.x.functionenv = paste(lgi.file.prefix, "-FUNCTION", sep="")

  if(file.exists(lgi.x.globalenv)) load(lgi.x.globalenv)
  
  for(package.name in rev(lgi.packages)) {
    cat("loading package", package.name, "\n")
    library(package.name, character.only=TRUE)
  }
  mycall <- get("lgi.call")
  # if parApply
  if(file.exists(lgi.x.functionenv)) {
    cat("loading function name")
    f.env <- new.env(parent=lgi.x.globalenv()) 
    load(lgi.x.functionenv, envir=f.env)
    if(PAR) {
      environment(mycall\$FUN) <- f.env
    } else {
      environment(mycall[[1]]) <- f.env
    }

    #set the apply value for X to be X
    if(! is.null(mycall\$X) ) { 
      warning("overwriting an existing X in the call with task copy")
    if(lgi.x.par) mycall\$X <- lgi.x.parX
  }
#  print(ls())
  eval(mycall)
})
#print(as.list(mycall))
#print(lgi.ret)
# upload result
save(lgi.ret, file=lgi.x.result)
#TODO system(paste("LGI_filetransfer", "upload", shQuote(repourl), shQuote(lgi.x.result)))


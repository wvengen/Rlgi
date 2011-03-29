# $Id: sge.get.result.R,v 1.2 2006/12/15 15:21:23 kuhna03 Exp $

sge.list.get.result <- function(list, ...) {
# this is just the result retrieval for sge.submit jobs
#  I dont like having this here, but its the best place, the end user should not have to make these calls
#print out everything from stderr, does not work if user changed stderr
  system(paste("for i in `ls *.e",list$jobid,"*`; do cat $i; done", sep=""))
  if(as.logical(getOption("sge.remove.files"))) {
    system(paste("rm *.e",list$jobid,"*; rm *.o", list$jobid, "*;" , sep=""))
  }
  sge.ret.name <- getOption("sge.ret.ext")
  sge.ret.ext <- paste(".", sge.ret.name, sep="")
  filename <- strsplit(list$filename, sge.ret.ext)[[1]]
  global.filename <- paste(filename, "-GLOBAL", sep="")
  function.filename <- paste(filename, "-FUNCTION", sep="")
  if (getOption("sge.remove.files") == TRUE) {
    file.remove(global.filename)
    if(file.exists(function.filename)) {
      file.remove(function.filename)
    }
  }
  sge.get.result(sge.fname=list$filename, jobid=list$jobid, ...)
}

"sge.get.result" <-function(sge.fname, jobid) {
#   I changed the filename to be the one to retrieve
#    sge.ret.name = getOption("sge.ret.ext")
#    sge.fname <- paste(filename,".", sge.ret.name, sep="")
    sge.ret.name <- getOption("sge.ret.ext") 
    sge.ret.ext <- paste(".", sge.ret.name, sep="")
    filename <- strsplit(sge.fname, sge.ret.ext)[[1]]
#cat("|",sge.fname,"|", filename ,"|\n")
    if (!file.exists(sge.fname)) {
      warning(paste("Expected file", sge.fname, "Does not exist at:", getwd()))
      return(NULL)
    }
# print out stderr from all of the jobs (this assumes that 
    
    load(sge.fname)
    if (getOption("sge.remove.files") == TRUE) {
      if(file.exists(filename)) {
        file.remove(filename)
      }
      file.remove(sge.fname)
    }
    ret <- get(sge.ret.name)
    if (class(ret) == "try-error")
      warning("Error(s) encountered in the remote R session")
    ret
  }

# $Id$

lgi.list.get.result <- function(list, ...) {
# this is just the result retrieval for lgi.submit jobs
#  I dont like having this here, but its the best place, the end user should not have to make these calls
#print out everything from stderr, does not work if user changed stderr
  # TODO print stdout/stderr
  #system(paste("for i in `ls lgi.",list$jobid,".stderr.txt`; do cat $i; done", sep=""))
  #if(as.logical(getOption("lgi.remove.files"))) {
  #  system(paste("rm lgi.",list$jobid,".stdout.txt lgi.", list$jobid, ".stderr.txt" , sep=""))
  #}
  lgi.ret.name <- getOption("lgi.ret.ext")
  lgi.ret.ext <- paste(".", lgi.ret.name, sep="")
  filename <- strsplit(list$filename, lgi.ret.ext)[[1]]
  global.filename <- paste(filename, "-GLOBAL", sep="")
  function.filename <- paste(filename, "-FUNCTION", sep="")
  if (getOption("lgi.remove.files") == TRUE) {
    file.remove(global.filename)
    if(file.exists(function.filename)) {
      file.remove(function.filename)
    }
  }
  lgi.get.result(lgi.fname=list$filename, jobid=list$jobid, ...)
}

"lgi.get.result" <-function(lgi.fname, jobid) {
#   I changed the filename to be the one to retrieve
#    lgi.ret.name = getOption("lgi.ret.ext")
#    lgi.fname <- paste(filename,".", lgi.ret.name, sep="")
    lgi.ret.name <- getOption("lgi.ret.ext") 
    lgi.ret.ext <- paste(".", lgi.ret.name, sep="")
    filename <- strsplit(lgi.fname, lgi.ret.ext)[[1]]
#cat("|",lgi.fname,"|", filename ,"|\n")
    if (!file.exists(lgi.fname)) {
      warning(paste("Expected file", lgi.fname, "Does not exist at:", getwd()))
      return(NULL)
    }
# print out stderr from all of the jobs (this assumes that 
    
    load(lgi.fname)
    if (getOption("lgi.remove.files") == TRUE) {
      if(file.exists(filename)) {
        file.remove(filename)
      }
      file.remove(lgi.fname)
    }
    ret <- get(lgi.ret.name)
    if (class(ret) == "try-error")
      warning("Error(s) encountered in the remote R session")
    ret
  }

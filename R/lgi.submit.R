# $Id: lgi.submit.R,v 1.3 2007/04/01 22:27:22 coultn Exp $

"lgi.submit" <- function(func, ..., 
                         application=getOption('lgi.application'),
                         global.savelist=NULL, 
                         function.savelist=NULL, 
                         packages=NULL,
                         debug=getOption('lgi.debug'),
                         file.prefix=getOption('lgi.file.prefix')
                         )
# savelist is a character vector of *names* of objects to be
# copied to the remote R session
{
   
  fname <- tempfile(pattern = file.prefix, tmpdir = getwd())
  lgi.globalPrep(func, ..., 
                 global.savelist=global.savelist,
                 function.savelist=function.savelist,
                 lgi.packages=packages,
                 debug=debug,prefix=fname
                )
  #lgi.call <- as.call(list(func, ...) )
  #lgi.packages <- packages
  #savelist <- c(savelist, "lgi.call", "lgi.packages")
  #save(list=savelist, file=fname)

  # and submit job
  program <- lgi.bootPrep(file.prefix, FALSE)
  files <- c(
    file.path(.path.package("Rlgi"), 'runjob.R'), # TODO upload with other name?
    paste(fname, "-GLOBAL", sep=''),
    paste(fname, "-FUNCTION", sep='')
  )
  result <- lgi.qsub(program, application, files[file.exists(files)])
  # we don't need these files anymore, they're uploaded anyway
  if (getOption("lgi.remove.files")) {
    unlink(paste(fname, "-GLOBAL", sep=''))
    unlink(paste(fname, "-FUNCTION", sep=''))
  }

  return(lgi.job.id(result))
}

# return lgi result for job id
# job must be finished or no results file is present
lgi.result <- function(jobid, fname=NA) {
  jobinfo = lgi.qstat(jobid)
  status = lgi.job.status(jobinfo)
  if (status!="finished") stop("Job must be finished to retrieve result")
  # We need the filename of the result. This could be obtained either
  # from the input (when lgi.submit was used), or from the repository's
  # file list (if the repository was used for just one job).
  # From a qstat joblist, however, none of these is returned.
  if (is.na(fname)) {
    for(c in xmlChildren(jobinfo[["job"]][["repository_content"]])) {
        if (xmlName(c)!="file") next
        name = xmlAttrs(c)[["name"]]
        regex = paste('^',getOption("lgi.prefix"),'.*-RESULT$', sep='')
        if (length(grep(regex, name))>0) {
          if (!is.na(fname)) stop("Multiple result files found in job repository")
          fname <- name
        }
    }
    if (is.na(fname)) stop("No result file found in job repository")
  }
  # now retrieve results file, get output, and remove file again
  lgi.file.get(lgi.job.repourl(jobinfo), fname)
  result <- paste(readLines(fname), collapse="\n")
  if (as.logical(getOption("lgi.remove.files"))) unlink(fname)
  return(result)
}


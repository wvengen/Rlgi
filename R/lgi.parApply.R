
                                        # $Id: lgi.parRapply.R,v 1.2 2006/12/15 15:21:23 kuhna03 Exp $

lgi.apply <- function(X, MARGIN, FUN, ..., 
                          join.method=cbind,
                          njobs,
                          batch.size=getOption('lgi.block.size'),
                          packages=NULL,
                          global.savelist=NULL,
                          function.savelist=NULL,
                          cluster=getOption('lgi.use.cluster'),
                          trace=getOption('lgi.trace'),
                          debug=getOption('lgi.debug'),
                          file.prefix=getOption('lgi.file.prefix')
                         ) {
    if(MARGIN == 1) {
      lgi.parRapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, 
                   global.savelist=global.savelist, 
                   function.savelist=function.savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix)
    } else {
      lgi.parCapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, 
                   global.savelist=global.savelist, 
                   function.savelist=function.savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix)
    } 
} 

lgi.parCapply <- function(X, FUN, ..., 
                          join.method=cbind,
                          njobs,
                          batch.size=getOption('lgi.block.size'),
                          packages=NULL,
                          global.savelist=NULL,
                          function.savelist=NULL,
                          cluster=getOption('lgi.use.cluster'),
                          trace=getOption('lgi.trace'),
                          debug=getOption('lgi.debug'),
                          file.prefix=getOption('lgi.file.prefix')
                         ) {
  if(cluster) {
    lgi.parParApply(t(X), FUN, ..., 
               join.method=join.method,  
               njobs=njobs, batch.size=batch.size,
               packages=packages, 
               global.savelist=global.savelist, 
               function.savelist=function.savelist,
               trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2
               )
  } else {
    if(trace) cat("Running locally \n")
    apply(X=X, MARGIN=2 ,FUN=FUN, ...)
  }
 
}

lgi.parRapply <- function(X, FUN, ...,
                          join.method=cbind,
                          njobs, 
                          batch.size=getOption('lgi.block.size'),
                          packages=NULL,
                          global.savelist=NULL,
                          function.savelist=NULL,
                          cluster=getOption('lgi.use.cluster'),
                          trace=getOption('lgi.trace'),
                          debug=getOption('lgi.debug'),
                          file.prefix=getOption('lgi.file.prefix')
                         ) {
  if(cluster) {
    lgi.parParApply(X, FUN, ...,  
                join.method=join.method, 
                njobs=njobs, batch.size=batch.size,
                packages=packages, 
                global.savelist=global.savelist, 
                function.savelist=function.savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2
                )
  } else {
    if(trace) cat("Running locally \n")
    apply(X=X, MARGIN=1 ,FUN=FUN, ...)
  }
}

lgi.parLapply <- function(X, FUN, ..., 
                          join.method=c, 
                          njobs,
                          batch.size=getOption('lgi.block.size'),
                          packages=NULL,
                          global.savelist=NULL,
                          function.savelist=NULL,
                          cluster=getOption('lgi.use.cluster'),
                          trace=getOption('lgi.trace'),
                          debug=getOption('lgi.debug'),
                          file.prefix=getOption('lgi.file.prefix')
                          ) {
  if(cluster) {
    lgi.parParApply(X, FUN, ...,
                join.method=join.method, njobs=njobs, 
                batch.size=batch.size,
                packages=packages, 
                global.savelist=global.savelist,
                function.savelist=function.savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=1
                )
  } else {
    if(trace) cat("Running locally\n")
    lapply(X=X, FUN=FUN, ...)
  }
}

# this code was blatently taken from snow, whose code was taken from sapply.R

lgi.parSapply <- function(X, FUN, ..., 
                          USE.NAMES=TRUE, simplify=TRUE,
                          join.method=c, 
                          njobs,
                          batch.size=getOption('lgi.block.size'),
                          packages=NULL,
                          global.savelist=NULL,
                          function.savelist=NULL,
                          cluster=getOption('lgi.use.cluster'),
                          trace=getOption('lgi.trace'),
                          debug=getOption('lgi.debug'),
                          file.prefix=getOption('lgi.file.prefix')
                         ) {
  
  if(cluster) {
    FUN <- match.fun(FUN) # should this be done on slave?
    answer <- lgi.parParApply(X, FUN, ...,
               join.method=join.method, njobs=njobs, 
               batch.size=batch.size,
               packages=packages, 
               global.savelist=global.savelist, 
               function.savelist=function.savelist,
               trace=trace, debug=debug, 
               file.prefix=file.prefix, apply.method=1
              )
#    answer <- lgi.parLapply(as.list(x), fun, ...)
      if (USE.NAMES && is.character(X) && is.null(names(answer)))
        names(answer) <- X
      if (simplify && length(answer) != 0) {
        common.len <- unique(unlist(lapply(answer, length)))
        if (common.len == 1)
            unlist(answer, recursive = FALSE)
        else if (common.len > 1)
            array(unlist(answer, recursive = FALSE),
                  dim = c(common.len, length(X)),
                  dimnames = list(names(answer[[1]]), names(answer)))
        else answer
      }
      else answer
  } else {
    if(trace) cat("Running locally\n")
    sapply(X=X, FUN=FUN, ..., simplify=simplify, USE.NAMES=USE.NAMES)
  } 
}

lgi.parParApply <- function (X, FUN, ...,
                           join.method=cbind,
                           njobs,
                           batch.size=getOption('lgi.block.size'),
                           trace=getOption('lgi.trace'),
                           packages=NULL,
                           global.savelist=NULL,
                           function.savelist=NULL,
                           debug=getOption('lgi.debug'),
                           file.prefix=getOption('lgi.file.prefix'),
                           apply.method=2
                         ) {
  # TODO move to args
  application=getOption('lgi.application')
  # split X
  if(missing(njobs) && (is.matrix(X) || is.data.frame(X)))
    njobs <- max(1,ceiling(nrow(X)/batch.size))    
  else if(missing(njobs) && (is.vector(X) || is.list(X)))
    njobs <- max(1,ceiling(length(X)/batch.size))    

  if(debug) print(X)
  if(njobs>1)
    rowSet <- lgi.split(X, njobs)
  else
    rowSet <- list(X)
  if(debug) print(rowSet)    
  prefix <- tempfile(pattern=file.prefix, tmpdir=getwd())
  # save the GLOBAL data
  if(apply.method == 1) {
    lgi.globalPrep(
                   lapply, X=NULL, FUN=FUN, ...,
                   global.savelist=global.savelist,
                   function.savelist=function.savelist,
                   lgi.packages=packages,
                   debug=debug,prefix=prefix
                  )
  } else if(apply.method ==2) {
    lgi.globalPrep(
                   apply, X=NULL, MARGIN=1, FUN=FUN, ...,
                   global.savelist=global.savelist,
                   function.savelist=function.savelist,
                   lgi.packages=packages,
                   debug=debug,prefix=prefix
                  )
  }
  if(trace) cat("Completed storing environment to disk\n")
  if(trace) cat("Submitting ",length(rowSet), "jobs...\n")
  # LGI knows no array jobs as of yet, so submit them one by one
  jobids = c()
  jobrepos = c()
  #jobsts = c()
  for (i in 1:length(rowSet)) {
    program <- lgi.bootPrep(prefix, TRUE, rowSet[[i]])
    result <- lgi.qsub(program, application)
    jobids = c(jobids, lgi.job.id(result))
    jobrepos = c(jobrepos, lgi.job.repourl(result))
    #jobsts = c(jobsts, lgi.job.status(result))
    # for now just transfer files to each of the children
    # TODO they should share common repository for environments
    lgi.file.put(lgi.job.repourl(result), c(
      paste(prefix, "-GLOBAL", sep=''),
      paste(prefix, "-FUNCTION", sep='')
    ))
  } 
  if(trace) cat("All jobs submitted, waiting for completion.\n") 
  # wait until all jobs are completed
  numqueued = numrunning = numother = 1
  while((numqueued+numrunning) > 0) {
    numqueued = numrunning = numother = 0
    for (c in xmlChildren(lgi.qstat())) {
			if (xmlName(c)!="job") next
      state = lgi.job.state(c)
      id = lgi.job.id(c)
      if (! id %in% jobids) next
      if (state=="queued") numqueued = numqueued + 1
      else if (state=="running") numrunning = numrunning + 1
		  else numother = numother + 1
      # TODO make sure we've had all job ids
    }
    if (trace) cat(paste("Waiting for jobs:", numqueued, "queued", numrunning, " running /", numother, "     \r"))
  }
  if (trace) cat("\n")

#  if(lgi.checkNotNow(result)) {
#    cat("now option set, could not run now on cluster, running local.\n")
#    if(apply.method == 1) {
#      return(lapply(X=X, FUN=FUN, ...))
#    } else {
#      return(apply(X=X, MARGIN=1, FUN=FUN, ...))
#    }
#  }
  if(trace) cat("All jobs completed\n") 
  # retrieve results from all jobs
  for(i in 1:length(rowSet)) {
    jobids = jobid[i]
    repourl = repourls[i]
    # TODO finish
  }

  # TODO output stdout/stderr
  # I am not sure how well R can handle this, maybe it will not scale
  #system(paste("for i in `ls *.e",jobid,"*`; do cat $i; done", sep=""))
  #if(as.logical(getOption("lgi.remove.files"))) {
  #  system(paste("rm *.e",jobid,"*; rm *.o", jobid, "*;" , sep=""))
  #}
  results <- lapply( filenames, lgi.get.result, jobid = jobid)
  if(as.logical(getOption("lgi.remove.files"))) file.remove(paste(prefix, "-GLOBAL",   sep=""))
  if(debug) print (results)
  # When c is run the try-errors are converted into strings
  # so its probably better to not combine errors, I
  # still need to seperately test this for cbind and consider other operations
  if(any(lapply(results , function(e1) class(e1) == "try-error") == TRUE)) {
    print("Not running join method since there are errors")
    results
  } else {
    retval <- docall(join.method, results)
    retval
  }
}

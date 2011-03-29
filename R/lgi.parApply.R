# $Id$

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
                          file.prefix=getOption('lgi.file.prefix'),
                          application=getOption('lgi.application')
                         ) {
    if(MARGIN == 1) {
      lgi.parRapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, 
                   global.savelist=global.savelist, 
                   function.savelist=function.savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix,
                   application=application)
    } else {
      lgi.parCapply(X, FUN, ...,
                   join.method=join.method,cluster=cluster,
                   njobs=njobs, batch.size=batch.size,
                   packages=packages, 
                   global.savelist=global.savelist, 
                   function.savelist=function.savelist,
                   trace=trace, debug=debug, file.prefix=file.prefix,
                   application=application)
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
                          file.prefix=getOption('lgi.file.prefix'),
                          application=getOption('lgi.application')
                         ) {
  if(cluster) {
    lgi.parParApply(t(X), FUN, ..., 
               join.method=join.method,  
               njobs=njobs, batch.size=batch.size,
               packages=packages, 
               global.savelist=global.savelist, 
               function.savelist=function.savelist,
               trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2,
               application=application
               )
  } else {
    lgi.debug("Running locally", debug=debug)
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
                          file.prefix=getOption('lgi.file.prefix'),
                          application=getOption('lgi.application')
                         ) {
  if(cluster) {
    lgi.parParApply(X, FUN, ...,  
                join.method=join.method, 
                njobs=njobs, batch.size=batch.size,
                packages=packages, 
                global.savelist=global.savelist, 
                function.savelist=function.savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=2,
                application=application
                )
  } else {
    lgi.debug("Running locally", debug=debug)
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
                          file.prefix=getOption('lgi.file.prefix'),
                          application=getOption('lgi.application')
                          ) {
  if(cluster) {
    lgi.parParApply(X, FUN, ...,
                join.method=join.method, njobs=njobs, 
                batch.size=batch.size,
                packages=packages, 
                global.savelist=global.savelist,
                function.savelist=function.savelist,
                trace=trace, debug=debug, file.prefix=file.prefix, apply.method=1,
                application=application
                )
  } else {
    lgi.debug("Running locally", debug=debug)
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
                          file.prefix=getOption('lgi.file.prefix'),
                          application=getOption('lgi.application')
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
               file.prefix=file.prefix, apply.method=1,
               application=application
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
    lgi.debug("Running locally", debug=debug)
    sapply(X=X, FUN=FUN, ..., simplify=simplify, USE.NAMES=USE.NAMES)
  } 
}

lgi.parParApply <- function (X, FUN, ...,
                           join.method=cbind,
                           njobs,
                           batch.size=getOption('lgi.block.size'),
                           packages=NULL,
                           global.savelist=NULL,
                           function.savelist=NULL,
                           debug=getOption('lgi.debug'),
                           trace=getOption('lgi.trace'),
                           file.prefix=getOption('lgi.file.prefix'),
                           apply.method=2,
                           application=getOption('lgi.application')
                         ) {
  # split X
  if(missing(njobs) && (is.matrix(X) || is.data.frame(X)))
    njobs <- max(1,ceiling(nrow(X)/batch.size))    
  else if(missing(njobs) && (is.vector(X) || is.list(X)))
    njobs <- max(1,ceiling(length(X)/batch.size))    

  lgi.trace('X:',X,trace=trace)
  if(njobs>1)
    rowSet <- lgi.split(X, njobs)
  else
    rowSet <- list(X)
  lgi.trace('rowSet:',rowSet, trace=trace)
  prefix <- tempfile(pattern=file.prefix, tmpdir=getwd())

  #if(trace) cat("Completed storing environment to disk\n")
  lgi.debug("Submitting",length(rowSet), "jobs...", debug=debug)
  # LGI knows no array jobs as of yet, so submit them one by one
  jobids = c()
  jobrepos = c()
  for (i in 1:length(rowSet)) {
    if(apply.method==1) {
      program <- lgi.prepareCall(
                     lapply, X=rowSet[[i]], FUN=FUN, ...,
                     global.savelist=global.savelist,
                     function.savelist=function.savelist,
                     lgi.packages=packages,
                     debug=debug,trace=trace,prefix=prefix
                    )
    } else if(apply.method==2) {
      program <- lgi.prepareCall(
                     apply, X=rowSet[[i]], MARGIN=1, FUN=FUN, ...,
                     global.savelist=global.savelist,
                     function.savelist=function.savelist,
                     lgi.packages=packages,
                     debug=debug,trace=trace,prefix=prefix
                    )
    }
    

    files <- c(
      paste(prefix, "-GLOBAL", sep=''),
      paste(prefix, "-FUNCTION", sep='')
    )
    result <- lgi.qsub(program, application, files[file.exists(files)])
    jobids = c(jobids, lgi.job.id(result))
    jobrepos = c(jobrepos, lgi.job.repourl(result))
    if (as.logical(getOption("lgi.remove.files"))) unlink(files)
  } 
  lgi.debug("All jobs submitted, waiting for completion.", debug=debug) 
  # wait until all jobs are completed
  numqueued = numrunning = numother = 1
  while((numqueued+numrunning) > 0) {
    numqueued = numrunning = numother = 0
    for (c in xmlChildren(lgi.qstat())) {
      if (xmlName(c)!="job") next
      state = lgi.job.status(c)
      id = lgi.job.id(c)
      if (! id %in% jobids) next
      if (state=="queued") numqueued = numqueued + 1
      else if (state=="running") numrunning = numrunning + 1
		  else numother = numother + 1
      # TODO make sure we've had all job ids
    }
    lgi.debug("Waiting for jobs:  ", numqueued, "queued;   ", numrunning, "running;  ", numother, "other      \r", endl=FALSE)
  }
  lgi.debug("")

#  if(lgi.checkNotNow(result)) {
#    cat("now option set, could not run now on cluster, running local.\n")
#    if(apply.method == 1) {
#      return(lapply(X=X, FUN=FUN, ...))
#    } else {
#      return(apply(X=X, MARGIN=1, FUN=FUN, ...))
#    }
#  }
  # TODO output stdout/stderr
  results <- lapply(jobids, lgi.result)
  lgi.trace('results:',results,trace=trace)
  # TODO update error handling wrt LGI
  # When c is run the try-errors are converted into strings
  # so its probably better to not combine errors, I
  # still need to seperately test this for cbind and consider other operations
  #if(any(lapply(results , function(e1) class(e1) == "try-error") == TRUE)) {
  #  print("Not running join method since there are errors")
  #  results
  #} else {
    retval <- docall(join.method, results)
    retval
  #}
}

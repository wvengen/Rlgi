# $Id$

#This will be coded so that savelists and packages can be sent to either.
"lgi.globalPrep" <- 
  function(func, ...,
           prefix,
           global.savelist=NULL,
           function.savelist=NULL,
           lgi.packages=NULL, 
           debug=getOption("lgi.debug"),
           trace=getOption("lgi.trace")
  ) 
  {
    # TODO test that lgi.x.* is not in global savelist
 
    if (is.null(lgi.packages)) lgi.packages = vector()
    # save the function call, use either global.savelist or globalenv()
    lgi.call <- as.call(list(func, ...) )
    if(is.null(global.savelist)) {
      if(as.logical(getOption("lgi.save.global"))) {
        lgi.debug("Saving entire global environment\n", debug=debug)
        global.savelist = ls(globalenv())
      } else {
        global.savelist = vector()
      }
    }

    #use either FUN if func is an apply call, or use func  
    if(is.null(lgi.call$FUN)) {
      e1 <- environment(lgi.call[[1]])
    } else {
      e1 <- environment(lgi.call$FUN)  
    } 

    if(!is.null(function.savelist)) {
      #create FUNCTION save file if we are using function.savelist
      function.name <- paste(prefix, "-FUNCTION",   sep="") 
      #set current environment to be global (gets rid of local enviroment)
      if(is.null(lgi.call$FUN)) {
        environment(lgi.call[[1]]) <- new.env(parent = globalenv())
      } else {
        environment(lgi.call$FUN) <-  new.env(parent = globalenv())
      }
      #save the function environment, this will be loaded into the function
      save(list=function.savelist, file=function.name, envir=e1)
    } #else {
      #c1 <- ls(environment(lgi.call$FUN))
      #iterate through all local environments
      #if(debug) print(c1)
      #if(debug) print(savelist)
      #if(environmentName(environment(lgi.call$FUN)) != environmentName(globalenv())) {
      #  lapply(savelist[savelist %in% c1], function(x) {
      #  print(paste("Variable", x, "was found in both the savelist and local environment, it will be written twice" ))
      # })
     #}
      # I should also test that there are no collisions between the arguments and scope
    #}

    lgi.debug("Global Environment variable:", global.savelist, debug=debug)
    lgi.debug("Packaged:", lgi.packages, debug=debug)
    if("lgi.call" %in% global.savelist) 
      warning("lgi.call is a reserved variable name, it cannot be passed to the global environmnet")
    if("lgi.packages" %in% global.savelist) 
      warning("lgi.packages is a reserved variable name, it cannot be passed to the global environmnet")
    #save globalenv to load later
    savelist <- c(global.savelist, "lgi.call", "lgi.packages")
    global.filename <-paste(prefix, "-GLOBAL", sep="")
    save(list=savelist, file=global.filename)
  }

"lgi.prepareCall" <- function(func, ...,
                              prefix,
                              comment=NA,
                              global.savelist=NULL,
                              function.savelist=NULL,
                              lgi.packages=vector(), 
                              debug=getOption("lgi.debug"),
                              trace=getOption("lgi.trace")) {

  result <- paste(basename(prefix), '-RESULT', sep='')
  resultqt <- gsub("'", "\\\\'", shQuote(result))
  global.filename <- paste(prefix, '-GLOBAL', sep='')
  function.filename <- paste(prefix, '-FUNCTION',   sep='') 

  # global environment
  lgi.x.call <- as.call(list(func, ...) )
  if(is.null(global.savelist)) {
    if(getOption("lgi.save.global")==TRUE) {
      lgi.debug("Saving entire global environment", debug=debug)
      global.savelist = ls(globalenv())
    } else {
      global.savelist = vector()
    }
  }

  # save function environment when present
  if(!is.null(function.savelist)) {
    # please look at Rsge for details, don't know exactly what happens here
    if(is.null(lgi.x.call$FUN)) {
      e1 <- environment(lgi.x.call[[1]])
      environment(lgi.x.call[[1]]) <- new.env(parent = globalenv())
    } else {
      e1 <- environment(lgi.x.call$FUN)  
      environment(lgi.x.call$FUN) <-  new.env(parent = globalenv())
    } 
    save(list=function.savelist, file=function.filename, envir=e1)
  }

  if("lgi.x.call" %in% global.savelist) 
    warning("lgi.x.call is a reserved variable name, it cannot be passed to the global environmnet")
  savelist <- c(global.savelist, 'lgi.x.call')
  save(list=savelist, file=global.filename)

  prog <- paste(
    paste(sapply(rev(lgi.packages), function(x){paste("library(",x,", character.only=TRUE)\n", sep='')}), collapse=''),
    "load('", basename(global.filename), "')\n",
    ifelse(!is.null(function.savelist), paste("load('", basename(function.filename), "')\n", sep=''), ''),
    ifelse(as.logical(debug), "print(lgi.x.call)\n", ''),
    "lgi.x.result <- eval(lgi.x.call)\n",
    ifelse(as.logical(debug), "print(lgi.x.result)\n", ''),
    "save(lgi.x.result, file='", result, "')\n",
    "system('LGI_filetransfer -j . upload `xml repository_url <LGI_job_specifics` ", resultqt, "')\n",
      sep='')
  if (!is.na(comment)) prog <- paste("# ", comment, "\n", prog, sep='')
  lgi.trace("Global environment:", global.savelist, trace=trace)
  if(!is.null(function.savelist)) lgi.trace("Function environment:", function.savelist, trace=trace)
  lgi.trace("Program to submit:\n", prog, trace=trace)
  return(prog)
}


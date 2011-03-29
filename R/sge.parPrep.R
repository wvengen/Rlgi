# $Id: sge.submit.R,v 1.3 2007/04/01 22:27:22 coultn Exp $
#This function has been deprecated. It was split into task and global Prep
#"sge.parPrep" <-
#  function(func, ..., 
#           savelist=c(), sge.packages=NULL, 
#           index=1, debug=getOption("sge.debug"), prefix=getOption("sge.file.prefix")
#  )
#  # savelist is a character vector of *names* of objects to be
#  # copied to the remote R session
#  {
## Extend the filenames to have the task array index 
#    if("sge.call" %in% savelist) stop("Cannot include X in savelist, it is a reserved variable name")
#    if("sge.packages" %in% savelist) stop("Cannot include X in savelist, it is a reserved variable name")
#    if(debug) print(paste("Packaged:", sge.packages))
#    fname <- paste(prefix, "-", index,  sep="")
#    sge.call <- as.call(list(func, ...) )
#    savelist <- c(savelist, "sge.call", "sge.packages")
#    save(list=savelist, file=fname)
#    sge.ret.name <- getOption("sge.ret.ext")
#    sge.fname <- paste(fname,".", sge.ret.name, sep="")
#    sge.fname 
#  }
#This will be coded so that savelists and packages can be sent to either.
"sge.globalPrep" <- 
  function(func, ...,
           prefix,
           global.savelist=NULL,
           function.savelist=NULL,
           sge.packages=NULL, 
           debug=getOption("sge.debug")
  ) 
  {
#test that there is no variable in savelist called X
 
# save the function call, use either global.savelist or globalenv()
    sge.call <- as.call(list(func, ...) )
    if(is.null(global.savelist) && (getOption("sge.save.global") == TRUE)) {
      if(debug) print("Saving entire global environment")
      global.savelist = ls(globalenv())

    }

#use either FUN if func is an apply call, or use func  
    if(is.null(sge.call$FUN)) {
      e1 <- environment(sge.call[[1]])
    } else {
      e1 <- environment(sge.call$FUN)  
    } 

    if(! is.null(function.savelist)) {
      #create FUNCTION save file if we are using function.savelist
      function.name <- paste(prefix, "-FUNCTION",   sep="") 
      #set current environment to be global (gets rid of local enviroment)
      if(is.null(sge.call$FUN)) {
        environment(sge.call[[1]]) <- new.env(parent = globalenv())
      } else {
        environment(sge.call$FUN) <-  new.env(parent = globalenv())
      }
      #save the function environment, this will be loaded into the function
      save(list=function.savelist, file=function.name, envir=e1)
    } #else {
      #c1 <- ls(environment(sge.call$FUN))
      #iterate through all local environments
      #if(debug) print(c1)
      #if(debug) print(savelist)
      #if(environmentName(environment(sge.call$FUN)) != environmentName(globalenv())) {
      #  lapply(savelist[savelist %in% c1], function(x) {
      #  print(paste("Variable", x, "was found in both the savelist and local environment, it will be written twice" ))
      # })
     #}
      # I should also test that there are no collisions between the arguments and scope
    #}

    if(debug) print(paste("Global Environment variable:", global.savelist))
    if(debug) print(paste("Packaged:", sge.packages))
    if("sge.call" %in% global.savelist) 
      warning("sge.call is a reserved variable name, it cannot be passed to the global environmnet")
    if("sge.packages" %in% global.savelist) 
      warning("sge.packages is a reserved variable name, it cannot be passed to the global environmnet")
    #save globalenv to load later
    savelist <- c(global.savelist, "sge.call", "sge.packages")
    global.filename <-paste(prefix, "-GLOBAL",   sep="")
    save(list=savelist, file=global.filename)
  }
#for par jobs just store the X variable
"sge.taskPrep" <- 
  function(X, index=1, debug=getOption("sge.debug"), prefix=getOption("sge.file.prefix")
          )
  {
    #if("X" %in% savelist) stop("Cannot include X in savelist, it is a reserved variable name")
    fname <- paste(prefix, "-", index,  sep="")
    #savelist <- c(savelist, "X", "packages")
    savelist <- c("X")
    save(list=savelist, file=fname)
    sge.ret.name = getOption("sge.ret.ext")
    sge.fname <- paste(fname,".", sge.ret.name, sep="")
  } 

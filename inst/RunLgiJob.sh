#!/bin/bash 

#might not use this var
#[ "$RSGE_R" ] || RSGE_R=R
RSGE_R=R
#check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <data file> [R path]"
    exit 1
fi
#not going to check this
#if [ -z "$LSB_JOBID" ]; then
#    echo "Not running under SGE ... aborting."
#    exit 1
#fi
sgeglobalenvfile=$1-GLOBAL
sgefunctionenvfile=$1-FUNCTION
if [ "$SGE_TASK_ID" != "undefined" ]; then
  sgeenvfile=$1-$SGE_TASK_ID; shift
else
  sgeenvfile=$1; shift
fi

sgereturnfile=${sgeenvfile}.sge.ret
#I am not exactly sure what this is going
[ "$1" ] && RSGE_R="$1"
#I am not going to support MPI, I dont understand what the MPI use case is.
#numhosts=0
#for i in $LSB_HOSTS; do
#    numhosts=`expr $numhosts + 1`
#done
#
#if [ $numhosts -gt 1 ]; then
#    bhostfile="Rsge_bhosts.$LSB_JOBID"
#    rm -f $bhostfile
#    for i in $LSB_HOSTS; do
#	echo $i >> $bhostfile
#    done
#    lamboot -v $bhostfile
#fi

"$RSGE_R" --vanilla <<EOF

sge.ret <- try({
#parApply branch  
  PAR <- FALSE
  if(file.exists("${sgeglobalenvfile}")) {
    load("${sgeglobalenvfile}")
  } 
  if(file.exists("${sgeenvfile}")) {
    #if this file exists, then we are running in parallel
    PAR <- TRUE
    load("${sgeenvfile}")
  }
  for(package.name in rev(sge.packages))
    {
	cat("loading package", package.name, "\n")
	library(package.name, character.only=TRUE)
8   }
  mycall <- get("sge.call")
  #we are running paralell version
  if(file.exists("${sgeglobalenvfile}")) {
    if(file.exists("${sgefunctionenvfile}")) {
      cat("loading function name")
      f.env <- new.env(parent=globalenv()) 
      load("${sgefunctionenvfile}", envir=f.env)
      if(PAR) {
        environment(mycall\$FUN) <- f.env
      } else {
        environment(mycall[[1]]) <- f.env
      }
    }

    if(! is.null(mycall\$X) ) { 
      warning("overwriting an existing X in the call with task copy")
    }
    #set the apply value for X to be X
    if(PAR) {
      mycall\$X <- X
    }
  }
#  print(ls())
  eval(mycall)
})
#print(as.list(mycall))
#print(sge.ret)
save(sge.ret, file = "${sgereturnfile}")

EOF

rc=$?
#not supporting MPI
#if [ $numhosts -gt 1 ]; then
#    lamhalt -v
#    rm -f $bhostfile
#fi

exit $rc


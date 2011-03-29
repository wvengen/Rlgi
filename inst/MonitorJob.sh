#!/bin/bash
#
# This script will monitor a running job and return its status.
#
# run qstat -j ... to determine if a job is currently running, once
# the job has completed, then use qacct to figure out what happened.
#
# qacct -j returns 0 if a job is successful, it returns other return codes to 
# specify other kinds of errors.
# returns 1 if the job is still running
# returns 1 = still running
# 0 = completed successfully
# other returns returns specified by qacct (see docs)

JOB_ID=$1
USE_QACCT=$2
DBG=$3
if [ "$4" ];then
  QSTAT=$4
else 
  QSTAT="qstat"
fi
if [ "$5" ];then
  QACCT=$5
else
  QACCT="qacct"
fi


if [ "$DBG" == "FALSE" ]; then
  exec >& /dev/null
fi
$QSTAT -j $JOB_ID > /dev/null 
if [ $? == 0 ]; then
  echo "STILL RUNNING"
  exit 1
else
  QACCT_RET=0
  echo "Not Running"
  if [ "$USE_QACCT" == "TRUE" ]; then
    $QACCT -j $JOB_ID > /dev/null
    QACCT_RET=$?
    if [ $QACCT_RET == 0 ]; then
      echo "Completed Successfully"
    fi
  fi
  exit $QACCT_RET
fi

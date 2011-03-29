#!/usr/bin/perl
#
# This script just cheks to see if any devices that are specified are using too much memory
# I just barely starting thinking about this, I will do this later.
#
use strict;


my $JOB_ID   = $ARGV[0]
my $values   = "free_mem";
my $interval = 60; 
my $out1 = `qstat`;
my $out2  = `qstat -F $values;
my @hosts = ();
foreach (@$out1) {
  if(/\s*$JOB_ID/) {
    push(split    
  } 
} 

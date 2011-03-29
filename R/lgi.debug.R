# $Id$

lgi.debug <- function(..., debug=getOption("lgi.debug"), endl=TRUE) {
  if (debug!=TRUE) return()
  cat('[Rlgi] ')
  cat(paste(...))
  if (endl) cat("\n")
}

lgi.trace <- function(..., trace=getOption("lgi.trace"), endl=TRUE) {
  lgi.debug(..., debug=trace, endl=endl)
}


library(Rlgi)

func1 <- function(x, y) {
  y[[x]] + 1
}


l1 <- list(a=1, b=2, c=3)
sapply(names(l1), func1, l1 )
lgi.parSapply(names(l1), func1, l1)

lgi.parSapply(names(l1), func1, l1, cluster=FALSE)
lgi.options(lgi.trace="FALSE")
lgi.parSapply(names(l1), func1, l1)
lgi.options(lgi.debug="TRUE")
lgi.options(lgi.trace="TRUE")
lgi.parSapply(names(l1), func1, l1)


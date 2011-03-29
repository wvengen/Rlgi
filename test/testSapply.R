library(Rsge)

func1 <- function(x, y) {
  y[[x]] + 1
}


l1 <- list(a=1, b=2, c=3)
sapply(names(l1), func1, l1 )
sge.parSapply(names(l1), func1, l1)

sge.parSapply(names(l1), func1, l1, cluster=FALSE)
sge.options(sge.trace="FALSE")
sge.parSapply(names(l1), func1, l1)
sge.options(sge.debug="TRUE")
sge.options(sge.trace="TRUE")
sge.parSapply(names(l1), func1, l1)


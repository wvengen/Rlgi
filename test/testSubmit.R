library(Rsge)
v1 <- c(1,2,3,4,5,6,7,8,9)
func1 <- function(x, y=2) {
  Sys.sleep(y) 
  x+7
}

x1 <- lapply(X=v1, FUN=func1, 1)

l1 <- list(length=length(v1))
for(i in 1:length(v1)) {
  l1[[i]] <- sge.submit(func1, v1[[i]])
}

r1 <- lapply(l1, sge.job.status) 
while(! all(r1 == 0)) {
  Sys.sleep(4)
  r1 <- lapply(l1, sge.job.status)
}

x1Par <- lapply(l1, sge.list.get.result)

#savelist/package list test

GLOBAL1=77
GLOBAL2= list(a=100,b=200,c=300) 
f2 <- function(x) GLOBAL1 + GLOBAL2$b + x
r2 <- f2(2)
l2 <- sge.submit(f2, 2, global.savelist=c("GLOBAL1", "GLOBAL2"), file.prefix="YOYO")
x <- sge.job.status(l2$jobid )
while(! x == 0) {
  Sys.sleep(4)
  x <- sge.job.status(l2$jobid)
}
r2Par <- sge.list.get.result(l2)

library("nlme")
f3 <- function(x) mean(Milk[[1]]) + GLOBAL2$c + x
r3 <- f3(3)
l3 <- sge.submit(f3, 3, packages=c("nlme"), file.prefix="YOYO")
x <- sge.job.status(l3$jobid)
while(! x == 0) {
  Sys.sleep(4)
  x <- sge.job.status(l3$jobid)
}
r3Par <- sge.list.get.result(l3)

# tests for environments


q1 = function(y) {
  f2 = function(x, y) x+y
  f3 = function(x) {
    f2(x,y)
  }
  f3(1)
}
s1 <- q1(1)


q1S = function(y) {
  f2 = function(x, y) x+y
  f3 = function(x) {
    f2(x,y)
  }
  info <- sge.submit(f3, 1)
  status <- sge.job.status(info$jobid)
  while(status != 0) {
    Sys.sleep(4)
    status <- sge.job.status(info$jobid)
  }
  sge.list.get.result(info)
}
s1Par <- q1S(1)

GLOBAL3 <- 3

g8Par = function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3
      sge.submit(f5, 1, global.savelist=c("GLOBAL3"), function.savelist=c())
    }
    f4()
  }
  f3()
}
info <- g8Par(1)
status <- sge.job.status(info$jobid)
while(status != 0) {
  Sys.sleep(4)
  status <- sge.job.status(info$jobid)
}
s2Par <- sge.list.get.result(info)
ERR1 <- class(s2Par) ==  "try-error"


g9Par <- function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3
      sge.submit(f5, 1, global.savelist=c("GLOBAL3"), function.savelist=c("f4", "l3", "l2", "l1", "x"))
    }
    f4()
  }
  f3()
}
info <- g9Par(1)
status <- sge.job.status(info$jobid)
while(status != 0) {
  Sys.sleep(4)
  status <- sge.job.status(info$jobid)
}
s3Par <- sge.list.get.result(info)

g9 <- function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3
      f5(1)
    }
    f4()
  }
  f3()
}
s3 <-g9(1)


print("TEST RESULTS")
all(x1Par == unlist(x1))
all(r2 == r2Par)
all(r3 == r3Par)
all(s1 == s1Par)
ERR1
s3Par == s3

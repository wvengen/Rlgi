library(Rsge)
#sge.options(sge.remove.files="FALSE")
# This script has all of the test that I ran on environments 
# to determine the required SGE support
#

# local environment is saved by default.

f1 = function(y) {
  f2 = function(x, y) x+y
  f3 = function(x) {
    f2(x,y)
  }
  sge.parLapply(X=c(1:6), FUN=f3, njobs=3)
}
r1Par <- f1(1)

f2 = function(y) {
  f2 = function(x, y) x+y
  f3 = function(x) {
    f2(x,y)
  }
  lapply(X=1:6, FUN=f3)
}
r1    <- f2(1)

#global environment tests
GLOBAL1 = 1

f3 = function(y) {
  f6 = function(x) {
    GLOBAL1 + x
  }
  lapply(1:3, f6)
}

r2 <- f3()

f4 = function(global.savelist=NULL) {
  f6 = function(x) {
    GLOBAL1 + x
  }
      #ls(environment(f5))
  sge.parLapply(1:3, f6, njobs=3, global.savelist=global.savelist)
}


sge.options(sge.save.global=TRUE)
# with an empty vector global env is not visable.
e1 = f4(global.savelist=vector())
ERR1 = any(lapply(e1 , function(e) class(e) == "try-error") == TRUE)
# global varialbe is visible by default
r2Par = f4()
sge.options(sge.save.global=FALSE)
# not saving global environment by default.
e2 <- f4()
ERR2 <- any(lapply(e2 , function(e) class(e) == "try-error") == TRUE)


#verify that the global environment is not passed to the sge par function call


GLOBAL2 = 10

g1 = function(x) x + GLOBAL2
r3 = g1(1)

# GLOBAL object needs to be explicitly added 

sge.options(sge.save.global=TRUE)
r3Par = sge.parLapply(c(1), g1)
r3.2Par = sge.parLapply(c(1), g1, global.savelist=c("GLOBAL2"))

# verify that a local environment created in g2/g3 will be passed 
# as a part of the function environment

g2 = function(x) {
  LOCAL1 <- 1
  f3 = function(y,z) y+z+LOCAL1
  f4 = function(a) f3(a,x)
  f4(2)
}
r4 = g2(2)

g3 = function(x, function.savelist=NULL) {
  LOCAL1 <- 1
  f3 = function(y,z) y+z + LOCAL1
  f4 = function(a, x) f3(a,x)
  sge.run(f4, 2, x, function.savelist=function.savelist)
}
r4Sub = g3(2)
r4.2Sub = g3(2, function.savelist=c("f3"))

#verify a mix of global and local environments
GLOBAL3 = 3
g4 = function(x) {
  f3 = function(y,z) y+z
  f4 = function(a) f3(a,x) + GLOBAL3
  lapply(100:104, f4)
}
r5 = g4(1)

g5 = function(x) {
  f3 = function(y,z) y+z
  f4 = function(a) f3(a,x) + GLOBAL3
  sge.parLapply(100:104, f4)
}
r5Par = g5(1)
sge.options(sge.save.global=FALSE)
e3 = g5(1)
ERR3 <- any(lapply(e3 , function(e) class(e) == "try-error") == TRUE)
sge.options(sge.save.global=TRUE)

# 
# Several levels of variables
#
g6 <- function(x) {
  l1 <- 100
  f3 <- function() {
    l2 <- 10
    f4 <- function(a) {
      a + l1 + l2 + x
    }
    lapply(1:9, f4)
  } 
  f3()
}
r6 <- g6(1)

g6Par <- function(x, function.savelist=NULL) {
  l1 <- 100
  f3 <- function() {
    l2 <- 10
    f4 <- function(a) {
      a + l1 + l2 + x
    }
    sge.parLapply(1:9, f4, njobs=3, function.savelist=function.savelist)
  }
  f3()
}
r6Par <- g6Par(1)
# verify that a save of some of the variabels from local
# env will make others not visible
e3 <- g6Par(1, function.savelist=c("f3", "l2"))
ERR4 <- any(lapply(e3 , function(e) class(e) == "try-error") == TRUE)

GLOBAL3 = 1000
g7 = function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3
      lapply(c(1.5, 2.6, 3.7, 4.8, 5.9, 6.05, 7.1, 8.2, 9.3, 10.4), f5)
    }
    f4()
  }
  f3()
}
r7 = g7(1)

g7Par = function(x, global.savelist=NULL, function.savelist=NULL) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3
      #print(global.savelist)
      sge.parLapply(
        c(1.5, 2.6, 3.7, 4.8, 5.9, 6.05, 7.1, 8.2, 9.3, 10.4), f5,
        global.savelist=global.savelist, function.savelist=function.savelist,
        njobs=4
      )
    }
    f4()
  }
  f3()
}
r7Par <- g7Par(1, global.savelist=c("GLOBAL3"))
e5 <- g7Par(1, global.savelist=vector())
e6 <- g7Par(1, function.savelist=vector())
e7 <- g7Par(1, global.savelist=vector(), function.savelist=vector())

ERR5 <- any(lapply(e5 , function(e) class(e) == "try-error") == TRUE)
ERR6 <- any(lapply(e6 , function(e) class(e) == "try-error") == TRUE)
ERR7 <- any(lapply(e7 , function(e) class(e) == "try-error") == TRUE)


GLOBAL8 <- 100
f8 <- function(x) x + GLOBAL8

g8 <- function() {
  GLOBAL8 <- 1000
  g9 <- function() {
    f8(GLOBAL8)
  }
  g9()
} 

r8 <- g8()

g8S <- function(function.savelist=NULL, global.savelist=NULL) {
  GLOBAL8 <- 1000
  g9 <- function() {
    f8(GLOBAL8)
  }
  sge.run(g9, function.savelist=function.savelist, global.savelist=global.savelist)
}

r8Sub <- g8S()
r8.2Sub <- g8S(function.savelist = c("GLOBAL8"))
r8.3Sub <- g8S(global.savelist = c("GLOBAL8", "f8"))

g9 <- function() {
  GLOBAL8 <- 1000
  g9 <- function(x) {
    x+f8(GLOBAL8)
  }
  lapply(1:10, g9)
}
r9 <- g9()

g9Par <- function(function.savelist=NULL, global.savelist=NULL) {
  GLOBAL8 <- 1000
  g9 <- function(x) {
    x+f8(GLOBAL8)
  }
  sge.parLapply(1:10, g9, function.savelist=function.savelist, global.savelist=global.savelist, njobs=3)
}

r9Par <- g9Par()
r9.2Par <- g9Par(function.savelist = c("GLOBAL8"))
r9.3Par <- g9Par(global.savelist = c("GLOBAL8", "f8"))


#you can either pass things from the environment 
# in the savelist or as arguments


g10Par = function(x, global.savelist=NULL, function.savelist=NULL) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z, l3, x) l1 + l2 + x + z + GLOBAL3
      sge.parLapply(
        c(1.5, 2.6, 3.7, 4.8, 5.9, 6.05, 7.1, 8.2, 9.3, 10.4), f5, l3, x, 
        njobs=4
      )
    }
    f4()
  }
  f3()
}
r10Par = g10Par(1, function.savelist=c("l1", "l2"))

library("nlme")

g12 = function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z) l1 + l2 + x + z + GLOBAL3 + mean(Milk[[1]])
      lapply(c(1.5, 2.6, 3.7, 4.8, 5.9, 6.05, 7.1, 8.2, 9.3, 10.4), f5)
    }
    f4()
  }
  f3()
}
r12 = g12(1)

g12Par = function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      l3 = 100000
      f5 = function(z, l3, x) {
        l1 + l2 + x + z + GLOBAL3 + mean(Milk[[1]]) 
      }
      sge.parSapply(
        c(1.5, 2.6, 3.7, 4.8, 5.9, 6.05, 7.1, 8.2, 9.3, 10.4), f5, l3, x,
        packages=c("nlme"), njobs=4
      )
    }
    f4()
  }
  f3()
}
r12Par = g12Par(1)

GLOBAL4 = 4
ft = function(x) x+GLOBAL4
# I got rid of these
#tryCatch(
#  {
#    sge.parLapply(c(1,2,3,4), ft, savelist=c("GLOBAL4", "sge.call"))
#    options(test.err1="FALSE")
#  }, error = function(ex) {
#    options(test.err1="TRUE")
#  }
#)#end try-catch
#tryCatch(
#  {
#    sge.parLapply(c(1,2,3,4), ft, savelist=c("GLOBAL4", "sge.packages"))
#    options(test.err2="FALSE")
#  }, error = function(ex) {
#    options(test.err2="TRUE")
#  }
#)#end try-catch
#tryCatch(
#  {
#    sge.parLapply(c(1,2,3,4), ft, savelist=c("GLOBAL4", "X"))
#    options(test.err3="FALSE")
#  }, error = function(ex) {
#    options(test.err3="TRUE")
#  }
#)#end try-catch


#test Rapply
g13 = function(x) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      m2 <- array(1:20, dim=c(4,5))
      f5 = function(z, x) l1 + l2 + x + z + GLOBAL3
      apply(
        X=m2, FUN=f5, MARGIN=1, x
      )
    }
    f4()
  }
  f3()
}
r13 = g13(1)


g13Par = function(x, global.savelist=NULL, function.savelist=NULL) {
  l1 = 100
  f3 = function() {
    l2 = 10
    f4 = function() {
      m2 <- array(1:20, dim=c(4,5))
      f5 = function(z, x) l1 + l2 + x + z + GLOBAL3
      sge.parRapply(
        m2, f5, x,
        global.savelist=global.savelist, function.savelist=function.savelist,
        njobs=4
      )
    }
    f4()
  }
  f3()
}
r13Par = g13Par(1)
r13.2Par = g13Par(1, global.savelist=c("GLOBAL3"))
r13.3Par = g13Par(1, function.savelist=c("l1", "l2"))


all(r1 == unlist(r1Par))
ERR1
all(r2 == unlist(r2Par))
ERR2
all(r3 == unlist(r3Par))
all(r3 == unlist(r3.2Par))
r4 == r4Sub
r4 == r4.2Sub
all(r5 == unlist(r5Par))
ERR3
all(r6 == unlist(r6Par))
ERR4
all(r7 == unlist(r7Par))
ERR5
ERR6
ERR7
r8 == r8Sub
r8 == r8.2Sub
r8 == r8.3Sub
all(r9 == unlist(r9Par))
all(unlist(r9) == r9.2Par)
all(r9 == unlist(r9.3Par))
all(r10Par == unlist(r7))
all(r12 == unlist(r12Par))
#getOption("test.err1")
#getOption("test.err2")
#getOption("test.err3")
all(r13 == r13Par)

source("hpSim.r")
source("parameters.r")

library(microbenchmark)

microbenchmark(
    R=simulatedHpReadLength (100, 5, allRates$C2["A",]),
    C=simulatedHpReadLengthC(100, 5, allRates$C2["A",]))


X <- c(rep(0,995), rep(1,5))

microbenchmark(
    A={ rbinom(100, 1000, 0.05) },
    B={ sample(X, 100, replace=T) })


draws <- simulatedHpReadLength(10000, 5, allParams$C2)
tbl   <- table(draws)


microbenchmark(
    A=simulatedHpReadLength(100, 5, allParams$C2),
    B=sample(as.integer(names(tbl)), 100, replace=T, prob=tbl/10000))

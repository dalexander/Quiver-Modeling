library(ggplot2)
library(reshape2)
library(plyr)

source("parameters.r")
sourceCpp("nativeCode.cpp")


# ------------------------------------------------------------------------------
# Simulate HP lengths

simulatedHpReadLength <- function(nObs, tplHpLen, params)
{
    ## Should we also handle model dark pulses?
    ## And what about noncognate extras?
    nBranches <- rbinom(nObs, tplHpLen, params[["Branch"]])
    nMerges   <- rbinom(nObs, tplHpLen-1, params[["Merge"]] + params[["Dark"]])
    nBranches - nMerges + tplHpLen
}

# -----------------------------------------------------------------------------
# Framework for benchmarking estimators

estimateHpAccuracyRate <- function(hpLen, coverage, estimator, params, mcReps=10000)
{
    estimates <- replicate(mcReps,
                           { reads <- simulatedHpReadLength(coverage, hpLen, params)
                             estimator(reads)
                           })
    sum(estimates==hpLen)/mcReps
    ## TODO: return montecarlo standard error as well
}


ALL_BASES <- c("A", "C", "G", "T")
COMPLEMENTS <- list(A="T", C="G", G="C", T="A")

estimateHpAccuracyRate.DS <- function(base, hpLen, coverage,
                                      dsEstimator,
                                      paramsByBase,
                                      mcReps=10000)
{
  ## DS = "double stranded"
  cmpBase <- COMPLEMENTS[[base]]
  params.fwd <- paramsByBase[base,]
  params.rev <- paramsByBase[cmpBase,]
  N.fwd <- ceiling(coverage/2)
  N.rev <- coverage - N.fwd
  estimates <- replicate(mcReps,
                         { reads.fwd <- simulatedHpReadLength(N.fwd, hpLen, params.fwd)
                           reads.rev <- simulatedHpReadLength(N.rev, hpLen, params.rev)
                           reads <- c(reads.fwd, reads.rev)
                           bases <- c(rep(base, N.fwd), rep(cmpBase, N.rev))
                           dsEstimator(reads, bases) })
  sum(estimates==hpLen)/mcReps
}

makeAccuracyTable <- function(params, coverageLevels, hpLens)
{
    estimatorMLE  <- makeEstimatorMLE (params)
    estimators <- list(MLE=estimatorMLE)
    configs <- expand.grid(Coverage=coverageLevels,
                           HpLength=hpLens,
                           Estimator=names(estimators))
    ddply(configs, .(HpLength,Coverage,Estimator), function(d) {
      data.frame(Accuracy=estimateHpAccuracyRate(d$HpLength,
                                                 d$Coverage,
                                                 estimators[[d$Estimator]],
                                                 params))
    })
}

makeAccuracyTable.DS <- function(paramsByBase, coverageLevels, hpLens)
{
    estimatorMLE  <- makeDsEstimatorMLE (paramsByBase)
    configs <- expand.grid(Coverage=coverageLevels,
                           HpLength=hpLens,
                           Base=ALL_BASES)
    ddply(configs, .(HpLength, Coverage, Base),
          function(d) {
              data.frame(Accuracy=estimateHpAccuracyRate.DS(
                             as.character(d$Base),
                             d$HpLength,
                             d$Coverage,
                             estimatorMLE,
                             paramsByBase))
          })
}

## This works in both cases!
coverageTitration <- function(hpLengthDist, accuracyTable)
{
  merged <- within(merge(accuracyTable,  hpLengthDist),
                   TotalErrorRate <- Frequency * (1-Accuracy))
  qvs <- ddply(merged, .(Coverage), summarize,
               QV = -10*log10(max(1/totalBases, sum(TotalErrorRate))))
  qvs
}

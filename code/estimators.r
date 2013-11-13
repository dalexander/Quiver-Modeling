library(Rcpp)
sourceCpp("nativeCode.cpp")

ALL_T = 0:16
ALL_R = 0:20
ALL_J = 0:10

singleLikelihood <- function(t, r, params)
{
    if (t <= 0) { 0 }
    else {
        x = r - t
        j = ALL_J
        branch_probs = dbinom(x+j,   t, params[["Branch"]])
        merge_probs  = dbinom(j,   t-1, params[["Merge"]])
        prob = branch_probs %*% merge_probs
        prob
    }
}

makeLikelihoodTable <- function(params, log.p=T)
{
    grid <- expand.grid(t=ALL_T, r=ALL_R)
    grid$likelihood <- mapply(singleLikelihood,
                              t=grid$t,
                              r=grid$r,
                              MoreArgs=list(params = params))
    if (log.p)
    {
        M <- log(acast(grid, t~r, value.var="likelihood"))
    } else {
        M <- acast(grid, t~r, value.var="likelihood")
    }
    M
}

makeEstimatorMLE.Slow <- function(params)
{
    M <- makeLikelihoodTable(params)
    function(hpReadLengths)
    {
        ##print(range(hpReadLengths))
        nReads <- length(hpReadLengths)
        hypothesisLLs <- rowSums(sapply(hpReadLengths, function(r) { M[ALL_T+1, r+1] }))
        as.integer(which.max(hypothesisLLs)) - 1
    }
}

makeEstimatorMLE <- function(params)
{
    llTbl <- makeLikelihoodTable(params)
    function(hpReadLengths)
    {
        mleEstimateC(llTbl, hpReadLengths)
    }
}


makeDsEstimatorMLE.Slow <- function(rates)
{
    Ms <- alply(rates, 1, .dims=T,
                .fun=function(ratesRow) {
                    makeLikelihoodTable(ratesRow)
                })
    function(hpReadLengths, hpBases) {
        nReads <- length(hpReadLengths)
        stopifnot(length(hpBases)==nReads)
        hypothesisLLs <- rowSums(mapply(function(r, base) {
                                            Ms[[base]][ALL_T+1, r+1]
                                        },
                                        hpReadLengths, hpBases))
        as.integer(which.max(hypothesisLLs)) - 1
    }
}



makeDsEstimatorMLE <- function(rates)
{
    Ms <- alply(rates, 1, .dims=T,
                .fun=function(ratesRow) {
                    makeLikelihoodTable(ratesRow)
                })
    function(hpReadLengths, hpBases) {
        dsMleEstimateC(Ms, hpReadLengths, hpBases)
    }
}


#
# For testing
#
if (0) {
    library(microbenchmark)
    source("parameters.r")

    sampleData <- c(rep(5, 10) , rep(6, 11))

    mleEstimator.Slow <- makeEstimatorMLE.Slow(allParams$P4C2)
    mleEstimator      <- makeEstimatorMLE     (allParams$P4C2)

    microbenchmark(
        mleEstimator.Slow(sampleData),
        mleEstimator     (sampleData))


    #
    # DS
    #
    e.slow <- makeDsEstimatorMLE.Slow(allRates$P4C2)
    e      <- makeDsEstimatorMLE     (allRates$P4C2)

    print(e(sampleData, rep("A", 21)))
    print(e(sampleData, rep("C", 21)))
    print(e(sampleData, rep("G", 21)))
    print(e(sampleData, rep("T", 21)))

    microbenchmark(
        e.slow(sampleData, rep("A", 21)),
        e     (sampleData, rep("A", 21)))

}

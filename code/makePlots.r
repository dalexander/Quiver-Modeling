library(ggplot2)
library(reshape2)
library(plyr)

source("parameters.r")
source("estimators.r")
source("hpSim.r")

options(device="quartz")

# ------------------------------------------------------------------------------
# Estimate accuracy params

ALL_COVERAGE <- c(5,10,20,30,40,50,60)

if (0) {
    cat("Recalculating HP accuracy tables, please wait\n")
    accTbls <- llply(allRates, makeAccuracyTable.DS,
                     coverageLevels=ALL_COVERAGE, hpLens=2:10,
                     .progress="text")
    save(accTbls, file="accTbls.rda")
} else {
    load("accTbls.rda")
}


# ------------------------------------------------------------------------------
# Compare chemistries (coverage titration)
#
source("empirical.r")
source("genomes.r")

estQVs<-ldply(accTbls, coverageTitration, hpLengthDist=hpLengthDist$Ecoli)

estQVs$Source = "Estimated"
qvs <- rbind(estQVs, empiricalQVs)

qplot(Coverage, QV, color=.id, linetype=Source, data=qvs) + geom_line()


# Look at merged tbl, can we see the error modes?
merged.P5C3.LowSNR <- within(merge(accTbls$P5C3.LowSNR, hpLengthDist$Ecoli),
                             TotalErrorRate <- Frequency * (1-Accuracy))

qplot(interaction(HpLength, Base), TotalErrorRate,
      data=subset(merged.P5C3.LowSNR, Coverage>=40), color=Coverage)


merged.P5C3.LowSNR.Swap <- within(merge(accTbls$P5C3.LowSNR.Swap, hpLengthDist$Ecoli),
                             TotalErrorRate <- Frequency * (1-Accuracy))

qplot(interaction(HpLength, Base), TotalErrorRate,
      data=subset(merged.P5C3.LowSNR.Swap, Coverage>=40), color=Coverage)




## What were the raw errors for P5C3.{Low,High}SNR?

ratesM <- lapply(allRates,
                 function(a) {
                     melt(a,  varnames=c("Base", "ErrorMode"),
                          value.name="Rate")
                 })

rates <- pbutils::collapse(ratesM, "Chemistry")

qplot(ErrorMode, Rate, fill=Base, geom="bar", stat="identity", data=rates, position="dodge") +
    facet_grid(Chemistry~.)

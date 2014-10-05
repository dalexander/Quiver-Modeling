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
estQVs$Source = "Inferred"

## qvs <- rbind(estQVs, empiricalQVs)
## qplot(Coverage, QV, color=.id, linetype=Source, data=qvs) + geom_line()

qplot(Coverage, QV, color=.id, linetype=Source, data=estQVs) +
    scale_linetype_manual(values=c("dashed")) +
    ggtitle("Inferred consensus accuracy") +
    geom_line()


## What were the raw errors?

ratesM <- lapply(allRates,
                 function(a) {
                     melt(a,  varnames=c("Base", "ErrorMode"),
                          value.name="Rate")
                 })
rates <- pbutils::collapse(ratesM, "Chemistry")

qplot(ErrorMode, Rate, fill=Base, geom="bar", stat="identity", data=rates, position="dodge") +
    facet_grid(Chemistry~.)


qplot(interaction(Base, ErrorMode), Rate, color=Chemistry, data=rates)

qplot(interaction(Base, ErrorMode), Rate, color=Chemistry, group=Chemistry,
      data=subset(rates, Chemistry %in% c("P4C2", "P5C3.1", "e11063P_DiSG1_8FMP"))) + geom_line()


qplot(interaction(Base, ErrorMode), Rate, fill=Chemistry, group=Chemistry,
      data=subset(rates, Chemistry %in% c("P4C2", "P5C3.1", "e11063P_DiSG1_8FMP")), geom="blank") +
    geom_bar(stat="identity", position="dodge")

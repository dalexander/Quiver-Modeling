library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(pbutils)

loadRates <- function(jobId)
{
    ##
    ## Find the EDNA csv, load the parameters.
    ##
    require(pbls)
    path <- fetchSecondaryFile(jobId, "edna.csv")
    if (identical(path, character(0)))
        stop("edna.csv not found");
    tbl <- subset(read.csv(path, nrows=100000),
                  select=c(Base, InsertOnA, InsertOnC, InsertOnG, InsertOnT, Merge, Dark))
    m <- melt(tbl, id.vars=c("Base"), variable.name="ErrorMode", value.name="Rate")
    rates <- ddply(m, .(Base, ErrorMode), summarize, Rate=mean(Rate))
    darkRates   <- subset(rates, ErrorMode=="Dark")
    mergeRates  <- subset(rates, ErrorMode=="Merge")
    branchRates <- transform(subset(rates, ErrorMode==str_c("InsertOn", Base)),
                             ErrorMode="Branch")
    rates <- rbind(darkRates, mergeRates, branchRates)
    acast(rates, Base~ErrorMode, value.var="Rate")
}

loadParams <- function(jobId)
{
    rates <- loadRates(jobId)
    colMeans(rates)
}

ednaJobs <- list(P4C2=186197,
                 C2=184047,
                 P5C3.LowSNR=196129,
                 P5C3.HighSNR=196134)

if (0) {
    allParams <- lapply(ednaJobs, loadParams)
    allRates  <- lapply(ednaJobs, loadRates)
    save(allParams, allRates, file="params.rda")
} else {
    load("params.rda")
}


if (1) {
    # Add the dye swap experiment
    allRates$P5C3.LowSNR.Swap <- allRates$P5C3.LowSNR[,]
    row.names(allRates$P5C3.LowSNR.Swap) <- c("A", "T", "G", "C")

    allRates$P5C3.HighSNR.Swap <- allRates$P5C3.HighSNR[,]
    row.names(allRates$P5C3.HighSNR.Swap) <- c("A", "T", "G", "C")
}

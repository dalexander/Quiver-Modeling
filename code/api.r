##
##
##

library(data.table)
library(plyr)
library(reshape2)
library(stringr)
library(Rcpp)


source("hpSim.r")
sourceCpp("nativeCode.cpp")

ALL_COVERAGE <- c(5,10,20,30,40,50,60)

# TODO: implement this
tabulateHomopolymerContent <- function(fastaFilename)
{
    if (fastaFilename == "Ecoli") {
        hpCounts <- read.csv("../resources/GenomeHpDistributions/ecoliK12_pbi_March2013.csv")
    } else {
        stop("This function doesn't really work yet.  Sorry.")
    }
    totalBases = sum(hpCounts$HpLength * hpCounts$Count)
    ddply(ecoliHps, .(HpLength, Base),
          summarize, Frequency=sum(Count)/totalBases)

}

calculateRawErrorRates <- function(file=NULL, jobid=NULL, tbl=NULL, sampleSize=10000)
{
    if (!is.null(tbl)) {
        # OK
    } else if (!is.null(file)) {
        tbl <- fread(file)
    } else if (!is.null(jobid)) {
        require(pbls)
        path <- fetchSecondaryFile(jobId, "edna.csv")
        if (identical(path, character(0)))
            stop("edna.csv not found--not an EDNA jobid?");
        tbl <- fread(file)
    } else {
        stop("Need a file or tbl or EDNA jobid")
    }
    sampleSize <- min(nrow(tbl), sampleSize)
    tbl <- subset(tbl[sample(1:nrow(tbl), sampleSize),],
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

calculateConsensusErrorRates <- function(rawErrorRates, coverageLevels=ALL_COVERAGE, estimator=NULL)
{
    makeAccuracyTable.DS(rawErrorRates, coverageLevels, 1:10)
}

predictAccuracy <- function(consensusErrorRates, hpLengthDist)
{
  merged <- within(merge(consensusErrorRates,  hpLengthDist),
                   TotalErrorRate <- Frequency * (1-Accuracy))
  qvs <- ddply(merged, .(Coverage), summarize,
               QV = -10*log10(max(1/totalBases, sum(TotalErrorRate))))
  qvs
}



## Example usage:
##
## rates <- calculateRawErrorRates(file="/home/UNIXHOME/dalexander/Projects/Analysis/CCS-Performance/EDNA/edna.csv")
## cssRates <- calculateConsensusErrorRates(rates)

## hpLengthDist <- tabulateHomopolymerContent("Ecoli")
## titration <- predictAccuracy(cssRates, hpLengthDist)

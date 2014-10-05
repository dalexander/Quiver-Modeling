library(ggplot2)

options(device="quartz")

source("api.r")

CCS_COVERAGE <- c(5, 10)

hpLengthDist <- tabulateHomopolymerContent("Ecoli")

fullTbl <- fread("/home/UNIXHOME/dalexander/Projects/Analysis/CCS-Performance/EDNA/edna.csv")

fullTbl$ReadName <- str_c(fullTbl$Movie, "/", fullTbl$HoleNumber, "/", fullTbl$ReadStart, "-", fullTbl$ReadEnd)
setkeyv(fullTbl, "ReadName")


titrationOnSnrCut <- function(A.min, A.max, T.min, T.max)
{
    goodReads.A <- subset(fullTbl, (Base=='A') & (PkmidToSigma >= A.min) & (PkmidToSigma < A.max))$ReadName
    goodReads.T <- subset(fullTbl, (Base=='T') & (PkmidToSigma >= T.min) & (PkmidToSigma < T.max))$ReadName
    intersection <- intersect(goodReads.A, goodReads.T)

    filtered <- fullTbl[J(intersection),]
    rawRates <- calculateRawErrorRates(tbl=filtered)
    cssRates <- calculateConsensusErrorRates(rawRates, coverageLevels=CCS_COVERAGE)

    titration <- predictAccuracy(cssRates, hpLengthDist)
    titration$A.min <- A.min
    titration$A.max <- A.max
    titration$T.min <- T.min
    titration$T.max <- T.max

    titration
}

# Define cuts
A.min <- c(0, seq(4, 10, by=2))
A.max <- c(A.min[-1], Inf)
A.cuts <- cbind(A.min, A.max)

T.min <- c(0, seq(4, 10, by=2))
T.max <- c(A.min[-1], Inf)
T.cuts <- cbind(T.min, T.max)

# From
# http://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))
snrCuts <- expand.grid.df(A.cuts, T.cuts)

topSnrCuts <- snrCuts[24:25,]

S <- ddply(snrCuts, .(A.min, A.max, T.min, T.max), splat(titrationOnSnrCut), .progress="text")

write.csv(S, "snrCuts.csv")



qplot(Coverage, QV, color=interaction(A.min, T.min), data=S) + geom_line()

qplot(Coverage, QV, data=S) + facet_grid(A.min ~ T.min) + geom_line()


qplot(as.ordered(Coverage), QV, data=S) + facet_grid(A.min ~ T.min) + geom_bar()


qplot(as.ordered(A.min), QV, data=subset(S, Coverage==10)) + facet_grid(. ~ T.min)

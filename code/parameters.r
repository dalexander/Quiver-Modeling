library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(pbutils)
library(data.table)

loadEdnaCsv <- function(jobId, sampleSize=10000)
{
   require(pbls)
   path <- fetchSecondaryFile(jobId, "edna.csv")
   if (identical(path, character(0)))
       stop("edna.csv not found");
   tbl <- fread(path)
   sampleSize <- min(nrow(tbl), sampleSize)
   tbl[sample(1:nrow(tbl), sampleSize),]
}

loadRates <- function(jobId)
{
    ##
    ## Find the EDNA csv, load the parameters.
    ##
    tbl <- subset(loadEdnaCsv(jobId),
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

## loadParams <- function(jobId)
## {
##     rates <- loadRates(jobId)
##     colMeans(rates)
## }


if (0) {
    #
    # This is all from MH jobs
    #   http://milhouse:8000/project/898/
    #   http://milhouse:8000/project/933/
    #
    ednaJobs <- list(P4C2                 = 207643,
                     P5C3.1               = 207617,
                     e11063P_C3_8pctFMP   = 207618,
                     e11063P_C4DB_8pctFMP = 207662,
                     e11063P_MonoSG1_4FMP = 207620,
                     e11063P_DiSG1_4FMP   = 207770)

    allRates  <- lapply(ednaJobs, loadRates)

    save(allRates, file="params.rda")

} else {
    load("params.rda")
}


avgs <- lapply(allRates, colMeans)

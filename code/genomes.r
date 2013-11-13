
## Genome homopolymer profiles, and also simulating a genome's HP
## breakdown by base composition

## want: genome$Ecoli$hpComposition -> table
##       genome$Ecoli$totalBases    -> int

ecoliHps <- read.csv(
    "/Users/dalexander/Dropbox/Sources/git/HpTools/length_distributions/ecoliK12_pbi_March2013.csv")
totalBases = sum(ecoliHps$HpLength * ecoliHps$Count)

hpLengthDist <- list(Ecoli=ddply(ecoliHps, .(HpLength, Base),
                     summarize, Frequency=sum(Count)/totalBases))

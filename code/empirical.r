
##
## Empirical results, for comparison to predictions
##

P4C2.Ecoli.Actual <- data.frame(
    .id = "P4C2",
    Coverage=c(20, 40, 60),
    QV=c(43,57,64))

empiricalQVs <- rbind(P4C2.Ecoli.Actual)
empiricalQVs$Source = "Empirical"

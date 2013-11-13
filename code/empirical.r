
##
## Empirical results, for comparison to predictions
##

P4C2.Ecoli.Actual <- data.frame(
    .id = "P4C2",
    Coverage=c(20, 40, 60),
    QV=c(43,57,64))

Dyeball.9566.Std.Actual <- data.frame(
    .id = "Dyeball.9566.Std",
    Coverage=c(20, 40, 60),
    QV=c(31, 35, 36))

empiricalQVs <- rbind(P4C2.Ecoli.Actual,
                      Dyeball.9566.Std.Actual)
empiricalQVs$Source = "Empirical"

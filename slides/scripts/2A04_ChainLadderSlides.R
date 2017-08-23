## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(ChainLadder)
GenIns
M <- MackChainLadder(GenIns, tail = TRUE)

## ------------------------------------------------------------------------
print(summary(M))
class(summary(M))
names(summary(M))
class(summary(M)$ByOrigin)
# Here is where the accident year ultimates are
summary(M)$ByOrigin$Ultimate
  # Aside: Using '$' sign same as pulling contents of "Ultimate" sublist
  summary(M)$ByOrigin[["Ultimate"]]
  # Contrast with the "Ultimate" sublist itself
  summary(M)$ByOrigin["Ultimate"]

# Back to business
class(summary(M)$Totals)
# Here is where total IBNR is
summary(M)$Totals["IBNR:", "Totals"]
summary(M)$Totals[4, 1]
# Here is the standard error of total IBNR
summary(M)$Totals["Mack S.E.:", "Totals"]

## ------------------------------------------------------------------------
DC <- ClarkLDF(GenIns)
print(summary(DC))
class(summary(DC))
# Here is where the accident year ultimates are
summary(DC)[1:10, "UltimateValue"]
# Here is where total IBNR is
summary(DC)["Total", "FutureValue"]
# Here is the standard error of total IBNR
summary(DC)["Total", "StdError"]

## ------------------------------------------------------------------------
# Premium sequence comes from Clark's paper
CC <- ClarkCapeCod(GenIns, Premium = 
                     seq(10e06, length = 10, by = 400000))
print(summary(CC))
class(summary(CC))
# Here is where the accident year ultimates are
summary(CC)[1:10, "UltimateValue"]
# Here is where total IBNR is
summary(CC)["Total", "FutureValue"]
# Here is the standard error of total IBNR
summary(CC)["Total", "StdError"]

## ------------------------------------------------------------------------
lnormParms <- function(mean, sd) {
  sigma2 <- log(1 + (sd / mean)^2)
  meanlog <- log(mean) - .5 * sigma2
  cbind(meanlog, sdlog = sqrt(sigma2))
}

## ------------------------------------------------------------------------
parms <- lnormParms(summary(M)$Totals["IBNR:", "Totals"],
           summary(M)$Totals["Mack S.E.:", "Totals"])
parms

## ------------------------------------------------------------------------
qlnorm(.95, parms[1], parms[2])

## ------------------------------------------------------------------------
plnorm(summary(M)$Totals["IBNR:", "Totals"], parms[1], parms[2])

## ------------------------------------------------------------------------
summary(M)$Totals["Mack S.E.:", "Totals"] / 
  summary(M)$Totals["IBNR:", "Totals"]

## ------------------------------------------------------------------------
summary(DC)["Total", "StdError"] / 
  summary(DC)["Total", "FutureValue"]
# Also found in the object
summary(DC)["Total", "CV"]


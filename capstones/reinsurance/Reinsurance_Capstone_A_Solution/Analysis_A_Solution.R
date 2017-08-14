# Analysis -- A Solution
library(tidyverse)
library(forecast)
library(e1071)
source("loadValidateData.R")
source("mean.py.wp.R")
source("burningCost.R")
source("reinsurancePricingFormula.R")
source("lognormal.R") # to help estimate the meanlog, sdlog parameters

set.seed(12)

# Add some calculated fields
policytbl <- mutate(policytbl, py = factor(year(policyeffdate)))
claimtbl <- mutate(claimtbl, ay = year(lossdate), rptd = paid + rsrv)

# losses by policy, most recent evaluation
##  First, pass through the claim table, most recent evaluation ...
eval_dtMostRecent <- max(claimtbl$eval_dt)
ct_agg <- claimtbl %>% 
  filter(eval_dt == eval_dtMostRecent) %>%
  group_by(policykey) %>%
  summarise(rptd = sum(rptd), paid = sum(paid), rsrv = sum(rsrv), n = n(),
            eval_dt = eval_dtMostRecent)
## Then join to policytbl and set no-match-policies to zero
policytbl <- left_join(policytbl, ct_agg, by = "policykey")
ind <- is.na(policytbl$n)
policytbl$n[ind] <- 0
policytbl$paid[ind] <- 0
policytbl$rsrv[ind] <- 0
policytbl$rptd[ind] <- 0
policytbl$eval_dt[ind] <- eval_dtMostRecent

# Policy year historical aggregates will be used for forecasting SEP, 
#   exposures, loss ratios, ...
# Reported Loss Ratio by PY. claim counts and exposure too
LR_rptd <- policytbl %>% group_by(py) %>%
  summarise(writtenpremium = sum(writtenpremium), rptd = sum(rptd),
            lr_rptd = rptd / writtenpremium, 
            n = sum(n), exposure = sum(exposure),
            lambda = n / exposure * 1e09)

# As a shortcut to avoid loss development issues, ignore the three more immature years
elr_rptd <- mean(head(LR_rptd$lr_rptd, 7))

# Calculate a corresponding average premium so as to get 
# average ceded premium rates down the road
wp <- policytbl %>% group_by(py) %>% summarise(wp = sum(writtenpremium))
wp
W <- mean(head(wp$wp, 7))


###################### XOL ###############

# Pulled these out of thin air
xol.retention <- 250000
xol.limit <- 750000

bc.XOL <- burningCostXOL.rptd(policytbl, xol.retention, 
                              xol.retention + xol.limit, "py")
E.XOL <- mean(head(bc.XOL, 7))
#E.XOL
#E.XOL / W
CedingCommission.XOL <- 0
BrokerageFee.XOL <- 0
InternalExpenseLoad.XOL <- .1
TargetReturn.XOL <- .1
P.XOL <- reinsurancePricingFormula(E.XOL, CedingCommission.XOL, BrokerageFee.XOL, 
                                   InternalExpenseLoad.XOL, TargetReturn.XOL)
RATE.XOL <- P.XOL / W
# for pretty number in report
P.XOLMillions <- paste0("$", round(P.XOL / 1000000, 1), "M")


###################### ASL ###############

# Pulled these out of thin air
asl.retention <- .4
asl.limit <- .3

bc.ASL <- burningCostASL.rptd(policytbl, asl.retention, 
                              asl.retention + asl.limit, "py")
E.ASL <- mean(head(bc.ASL, 7))
CedingCommission.ASL <- 0
BrokerageFee.ASL <- 0
InternalExpenseLoad.ASL <- .1
TargetReturn.ASL <- .1
P.ASL <- reinsurancePricingFormula(E.ASL, CedingCommission.ASL, BrokerageFee.ASL, 
                                   InternalExpenseLoad.ASL, TargetReturn.ASL)
RATE.ASL <- P.ASL / W
# for pretty number in report
P.ASLMillions <- paste0("$", round(P.ASL / 1000000, 1), "M")


########## SIMULATIONS #################

# run 25000 simulations of treaty year's underwriting result
# assume poisson claim count
# assume lognormal severity

# Poisson
# Average claim rate per exposure
lambda <- mean(head(LR_rptd$lambda, 7))
  
EXPOSURE <- c(forecast::forecast(LR_rptd$exposure, h = 1)$mean)
LAMBDA <- round(EXPOSURE / 1e09 * lambda, 0)

NUMSIM <- 25000

NUMCLAIMS <- rpois(NUMSIM, LAMBDA)

# SEVERITY
# lognormal
# Fit a lognormal to the distribution of reported losses for the
# most recent diagonal and accident years older than 2013
x <- (claimtbl %>% 
    filter(eval_dt == eval_dtMostRecent,
           as.numeric(as.character(ay)) < 2013))$rptd
LN_parms <- lnormParms(mean(x), sd(x))
MU <- LN_parms["mu"]
SIGMA <- LN_parms["sigma"]

# Subject Earned Premium for treaty year
SEP <- round(c(forecast::forecast(LR_rptd$writtenpremium, h = 1)$mean), -4)

# AGGREGATE LOSS for expected treaty year
# Set aside storage for the scenario results
TotalLosses <- numeric(NUMSIM)
TotalLossesInXOLLayer <- numeric(NUMSIM)
TotalLossesInASLLayer <- numeric(NUMSIM)

# For each trial, simulate that number of claims (from Poisson)
# and calculate total gross and ceded losses.
# Limited values use vectorized min and max functions
for (i in 1:NUMSIM){
  losses <- rlnorm(NUMCLAIMS[i], meanlog = MU, sdlog = SIGMA)
  lossesInXOLLayer <- pmax(
    pmin(
      xol.retention + xol.limit, losses) - 
      xol.retention, 
    0)
  TotalLossesInXOLLayer[i] <- sum(lossesInXOLLayer)
  TotalLosses[i] <- sum(losses)
  TotalLossesInASLLayer[i] <- max(
    min(
      (asl.retention + asl.limit) * SEP, TotalLosses[i]) - 
      asl.retention * SEP,
    0)
}
# Ceded premium net of recoveries
CPNOR.XOL <- SEP * RATE.XOL - SEP * CedingCommission.XOL - TotalLossesInXOLLayer
CPNOR.ASL <- SEP * RATE.ASL - SEP * CedingCommission.ASL - TotalLossesInASLLayer
# Net premium net of recoveries
NPNOR.BARE <- DPNOR.BARE <- SEP - TotalLosses
NPNOR.XOL <- NPNOR.BARE - CPNOR.XOL
NPNOR.ASL <- NPNOR.BARE - CPNOR.ASL

# Table of outcomes
# Put into a data.frame to be sorted by BARE results.
# Note: this way of sorting is not recommended by Venter, but done
#   here for simplicity.
# Can you find a solution that sorts bare and the two ceded results,
#   and corresponding net results, separately?
PNOR.DF <- data.frame(BARE = DPNOR.BARE, 
                      NetOfXOL = NPNOR.XOL, 
                      NetOfASL = NPNOR.ASL,
                      CPNOR.XOL = CPNOR.XOL,
                      CPNOR.ASL = CPNOR.ASL,
                      Loss.Dir = TotalLosses, 
                      LossRatio.Dir = round(TotalLosses * 100 / SEP, 4),
                      Loss.XOL = TotalLossesInXOLLayer,
                      Loss.ASL = TotalLossesInASLLayer)
PNOR.DF <- PNOR.DF[order(PNOR.DF$BARE), ]


# Table of summary statistics -- Gary's Table 1 (unlabeled)
NPNOR_Statistics <- data.frame(
  Statistic = c("Mean", "Standard Deviation","Skewness"),
  BARE = c(
    prettyNum(mean(NPNOR.BARE), big.mark = ","), 
    prettyNum(sd(NPNOR.BARE), big.mark = ","),
    prettyNum(e1071::skewness(NPNOR.BARE), digits = 4)),
  NetOfXOL = c(
    prettyNum(mean(NPNOR.XOL), big.mark = ","), 
    prettyNum(sd(NPNOR.XOL), big.mark = ","),
    prettyNum(e1071::skewness(NPNOR.XOL), digits = 4)),
  NetOfASL = c(
    prettyNum(mean(NPNOR.ASL), big.mark = ","), 
    prettyNum(sd(NPNOR.ASL), big.mark = ","),
    prettyNum(e1071::skewness(NPNOR.ASL), digits = 4))
)
# Use brute force to insert the 99th percentile
w99 <- round(.99 * NUMSIM, 0)
NPNOR_Statistics <- rbind(
  NPNOR_Statistics,
  data.frame(Statistic = "Safety Level, Percent",
             BARE = "99.0%",
             NetOfXOL = "99.0%",
             NetOfASL = "99.0%"),
  data.frame(Statistic = "Safety Level, Value", 
             data.frame(t(prettyNum(PNOR.DF[w99, 1:3], big.mark = ","))))
)

# Comparisons for the report
ASLoverXOLBenefit.99 <- PNOR.DF[w99, 3] - PNOR.DF[w99, 2]
ASLoverXOLBenefit.Worst <- PNOR.DF[NUMSIM, 3] - PNOR.DF[NUMSIM, 2]

# For density graph in the report
denB <- density(PNOR.DF$BARE)
denX <- density(PNOR.DF$NetOfXOL)
denA <- density(PNOR.DF$NetOfASL)

# For ecdf graph in the report
PNOR.trimmed <- 
  PNOR.DF[round(1/500 * NUMSIM, 0):round((1 - 1/500) * NUMSIM, 0), 1:3]

## Gary's last table
# First row cannot start at index 0
probs <- c(1 / NUMSIM, seq(.0025, .02, by = .0025),
           seq(.04, .5, by = .02))
w <- round(probs * NUMSIM, 0)
PNOR.DF[w, ]
Table2 <- cbind(Row = w, Probability = probs, PNOR.DF[w, 1:3])


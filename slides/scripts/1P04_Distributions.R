## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
# Generate 1000 random lognormal random variables with parameters mulog = 1, sdlog = .5
severity <- rlnorm(1000, 1, .5)

## ------------------------------------------------------------------------
# Generate 1000 random lognormal random variables with parameters mulog = 1, sdlog = .5
set.seed(12345)
severity <- rlnorm(1000, 1, .5)

## ------------------------------------------------------------------------
prob <- seq(.005, .995, by = .005)
x <- qnorm(prob, 0, 1)
y <- dnorm(x, 0, 1)
plot(x, y)

## ----pressure------------------------------------------------------------
# 4 graphs on a page, row-wise
op <- par(mfrow=c(2,2)) 
plot(x <- qnorm(prob <- seq(.005, .995, by = .005)), dnorm(x))
plot(x, y = pnorm(x))
plot(prob, qnorm(prob))
plot(rnorm(100), main = "white noise")
par(op) # reset to 1 graph per page

## ------------------------------------------------------------------------
# Simulate 25 gamma-distributed claims
# FYI for Gamma: E(X) = shape * scale, 
#    Var(X) = shape * scale^2, 
set.seed(12345)
clm <- rgamma(25, shape = 2, scale = 5000)
summary(clm)
op <- par(mfrow = c(3, 2)) #3 rows, 2 columns
plot(clm)
plot(ecdf(clm))
plot(density(clm))
hist(clm)
hist(clm, breaks = seq(range(clm)[1], range(clm)[2], length = 11))
par(op)

## ------------------------------------------------------------------------
N <- 10000 # number of trials
# Loop
TotalLossLoop <- numeric(N)
for (i in 1:N) {
  numclaims <- rpois(1, 100)
  claims <- rlnorm(numclaims, 6.5, .8)
  TotalLossLoop[i] <- sum(claims)
}
print(mean(TotalLossLoop))
# apply function
numclaims <- rpois(N, 100)
TotalLossApply <- sapply(numclaims, function(n) sum(rlnorm(n, 6.5, .8)))
print(mean(TotalLossApply))

## ------------------------------------------------------------------------
library(actuar)
lnormParms <- function(mean, std){
  stopifnot(length(mean) == length(std))
  if (any(mean < 0) | any(std < 0)) stop("Negative mean or std")
  sigma2 <- log(1 + (std / mean)^2)
  mu     <- log(mean) - .5 * sigma2
  cbind(mulog = mu, sdlog = sqrt(sigma2))[,]
  }
print(lnormParms(mean=1000, std=1000))
mlnorm(order = 1, meanlog = 6.5611817, sdlog = 0.8325546) # theoretical E(X)
levlnorm(1000, meanlog = 6.5611817, sdlog = 0.8325546, order = 1)

## ------------------------------------------------------------------------
mlnorm(1, meanlog = 7.9, sdlog = 1.5)

## ------------------------------------------------------------------------
levlnorm(100000, meanlog = 7.9, sdlog = 1.5) / levlnorm(25000, meanlog = 7.9, sdlog = 1.5)


## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(
    warning=FALSE
  , error=FALSE
  , echo=TRUE
  , message=FALSE
  , fig.height = 4.5
  , collapse = TRUE
)
knitr::opts_knit$set(root.dir = normalizePath('../../'))

## ------------------------------------------------------------------------
set.seed(8910)
years <- 2001:2010
frequency <- 1000

N <- rpois(length(years), frequency)

sevShape <- 2
sevScale <- 1000
severity <- rgamma(sum(N), sevShape, scale = sevScale)

## ----collapse=TRUE-------------------------------------------------------
mean(severity)
median(severity)
var(severity)
sd(severity)
quantile(severity, 0.25)
quantile(severity, c(0.25, 0.5, 0.75))

## ------------------------------------------------------------------------
summary(severity)

## ------------------------------------------------------------------------
hist(severity)

## ------------------------------------------------------------------------
plot(density(severity))

## ------------------------------------------------------------------------
library(MASS)

fitGamma <- fitdistr(severity, "gamma")
fitLognormal <- fitdistr(severity, "lognormal")
fitWeibull <- fitdistr(severity, "Weibull")

fitGamma
fitLognormal
fitWeibull

## ------------------------------------------------------------------------
probabilities = (1:(sum(N)))/(sum(N)+1)

weibullQ <- qweibull(probabilities, coef(fitWeibull)[1], coef(fitWeibull)[2])
lnQ <- qlnorm(probabilities, coef(fitLognormal)[1], coef(fitLognormal)[2])
gammaQ <- qgamma(probabilities, coef(fitGamma)[1], coef(fitGamma)[2])

sortedSeverity <- sort(severity)

## ----eval=FALSE----------------------------------------------------------
## oldPar <- par(mfrow = c(1,3))
## plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
## abline(0,1)
## 
## plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
## abline(0,1)
## 
## plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
## abline(0,1)
## 
## par(oldPar)

## ----echo=FALSE----------------------------------------------------------
oldPar <- par(mfrow = c(1,3))
plot(sort(weibullQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Weibull Fit")
abline(0,1)

plot(sort(lnQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Lognormal Fit")
abline(0,1)

plot(sort(gammaQ), sortedSeverity, xlab = 'Theoretical Quantiles', ylab = 'Sample Quantiles', pch=19, main = "Gamma Fit")
abline(0,1)

par(oldPar)

## ------------------------------------------------------------------------
sampleLogMean <- fitLognormal$estimate[1]
sampleLogSd <- fitLognormal$estimate[2]

sampleShape <- fitGamma$estimate[1]
sampleRate <- fitGamma$estimate[2]

sampleShapeW <- fitWeibull$estimate[1]
sampleScaleW <- fitWeibull$estimate[2]

x <- seq(0, max(severity), length.out=500)
yLN <- dlnorm(x, sampleLogMean, sampleLogSd)
yGamma <- dgamma(x, sampleShape, sampleRate)
yWeibull <- dweibull(x, sampleShapeW, sampleScaleW)

## ----eval=FALSE----------------------------------------------------------
## hist(severity, freq=FALSE, ylim=range(yLN, yGamma))
## 
## lines(x, yLN, col="blue")
## lines(x, yGamma, col="red")
## lines(x, yWeibull, col="green")

## ----echo=FALSE, fig.height=5.5------------------------------------------
hist(severity, freq=FALSE, ylim=range(yLN, yGamma))

lines(x, yLN, col="blue")
lines(x, yGamma, col="red")
lines(x, yWeibull, col="green")

## ------------------------------------------------------------------------
sampleCumul <- seq(1, length(severity)) / length(severity)
stepSample  <- stepfun(sortedSeverity, c(0, sampleCumul), f = 0)
yGamma <- pgamma(sortedSeverity, sampleShape, sampleRate)
yWeibull <- pweibull(sortedSeverity, sampleShapeW, sampleScaleW)
yLN <- plnorm(sortedSeverity, sampleLogMean, sampleLogSd)

## ----echo=FALSE,  fig.height=5.5-----------------------------------------
oldPar <- par(mfrow = c(3,1))
plot(stepSample, col="black", main = "K-S Gamma")
lines(sortedSeverity, yGamma, col = "blue")

plot(stepSample, col="black", main = "K-S Weibull")
lines(sortedSeverity, yWeibull, col = "blue")

plot(stepSample, col="black", main = "K-S Lognormal")
lines(sortedSeverity, yLN, col = "blue")
par(oldPar)

## ------------------------------------------------------------------------
testGamma <- ks.test(severity, "pgamma", sampleShape, sampleRate)
testLN <- ks.test(severity, "plnorm", sampleLogMean, sampleLogSd)
testWeibull <- ks.test(severity, "pweibull", sampleShapeW, sampleScaleW)

testGamma
testLN
testWeibull

## ------------------------------------------------------------------------
quadraticFun <- function(a, b, c){
  function(x) a*x^2 + b*x + c
}

myQuad <- quadraticFun(a=4, b=-3, c=3)

## ----echo=FALSE----------------------------------------------------------
plot(myQuad, -10, 10)

## ------------------------------------------------------------------------
myResult <- optim(8, myQuad)
myResult

## ------------------------------------------------------------------------
myOtherQuad <- quadraticFun(-6, 20, -5)
plot(myOtherQuad, -10, 10)

## ------------------------------------------------------------------------
myResult <- optim(8, myOtherQuad)
myResult
myResult <- optim(8, myOtherQuad, control = list(fnscale=-1))
myResult

## ------------------------------------------------------------------------
N <- 100
B0 <- 5
B1 <- 1.5
B2 <- 0.8

set.seed(1234)

e <- rnorm(N, mean = 0, sd = 1)
X1 <- runif(N, 0, 10)
X2 <- runif(N, 30, 60)
X3 <- runif(N, 200, 300)

Y <- B0 + B1 * X1 + B2 * X2 + e

## ------------------------------------------------------------------------
plot(X1, Y)

## ------------------------------------------------------------------------
fit <- lm(Y ~ X1)
summary(fit)

## ------------------------------------------------------------------------
prediction <- predict(fit)

## ----echo=FALSE----------------------------------------------------------
plot(X1, Y)
lines(X1, prediction, col="red")

## ------------------------------------------------------------------------
plot(Y, residuals(fit))
abline(0, 0, col="red")

## ------------------------------------------------------------------------
fit2 <- lm(Y ~ X1 + X2)
summary(fit2)

fit3 <- lm(Y ~ X1 + X2 + X3)
summary(fit3)

## ------------------------------------------------------------------------
severity <- 10000
CV <- .3
sigma <- sqrt(log(1 + CV^2))
mu <- log(severity) - sigma^2/2
plot(function(x) dlnorm(x), mu, sigma, ylab="LN f(x)")


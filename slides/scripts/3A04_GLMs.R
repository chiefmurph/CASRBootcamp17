## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(
    warning=FALSE
  , error=FALSE
  , echo=FALSE
  , message=FALSE
)
knitr::opts_knit$set(root.dir = normalizePath('../../'))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
set.seed(1234)

num_policies <- 5e3

payroll <- runif(num_policies, 500e3, 10e6)
years_in_operation <- rpois(num_policies, 5)
number_of_employees <- as.integer(runif(num_policies, 50, 2e3))
claims_per_mil <- 1.5 - .1 * years_in_operation + 0.001 * number_of_employees
lambda <- claims_per_mil * payroll / 1e6
claim_count <- rpois(num_policies, lambda)

dfGLM <- data.frame(
    ClaimCount = claim_count
  , Payroll = payroll
  , YearsInOperation = years_in_operation
  , NumberOfEmployees = number_of_employees)

total_claims <- sum(claim_count)
claim_severity <- rgamma(total_claims, 10, 1/10e3)
open_prob <- pmin(claim_severity / 500e3, 1)

dfBinomial <- data.frame(
      ClaimSeverity = claim_severity
    , OpenProb = open_prob
    , Open = rbinom(total_claims, 1, open_prob)
  )

## ----fig.height=4--------------------------------------------------------
plt <- ggplot(dfGLM, aes(ClaimCount)) + geom_histogram(binwidth = 1)
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(Payroll, ClaimCount)) + geom_hex()
plt <- plt + scale_x_continuous(labels = scales::dollar) + ylab("# of Claims")
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(as.factor(YearsInOperation), ClaimCount)) + geom_boxplot()
plt <- plt + ylab("# of Claims") + xlab('Years in operation')
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(NumberOfEmployees, ClaimCount)) + geom_hex()
plt <- plt + scale_x_continuous(labels = scales::comma)
plt <- plt + ylab("# of Claims") + xlab('Number of Employees')
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(Payroll, ClaimCount)) + geom_hex()
plt <- plt + scale_x_continuous(labels = scales::dollar) + ylab("# of Claims")
plt + geom_smooth(method = "lm", formula = y ~ 1 + x)

## ----echo=TRUE-----------------------------------------------------------
fit_lm <- lm(ClaimCount ~ Payroll, data = dfGLM)

## ----echo=TRUE-----------------------------------------------------------
fit_glm <- glm(ClaimCount ~ Payroll + YearsInOperation + NumberOfEmployees, data = dfGLM, family = "poisson")

## ------------------------------------------------------------------------
summary(fit_glm)

## ------------------------------------------------------------------------
dfGLM$Predict1 <- predict(fit_glm, type = 'response')

## ------------------------------------------------------------------------
dfGLM$Residual1 <- residuals(fit_glm, type = 'response')
plt <- ggplot(dfGLM, aes(Predict1, Residual1)) + geom_hex()
plt <- plt + geom_hline(yintercept = 0, color = 'red')
plt

## ----echo=TRUE, collapse=TRUE--------------------------------------------
fit_1 <- glm(ClaimCount ~ 1 + Payroll, data = dfGLM, family="poisson")

fit_2 <- glm(ClaimCount ~ 1, data = dfGLM, family="poisson", offset=log(Payroll))

fit_1$aic
fit_2$aic
coef(fit_1)
coef(fit_2)

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(ClaimSeverity, Open)) + geom_hex()
plt <- plt + scale_x_continuous(labels=scales::dollar)
plt

## ------------------------------------------------------------------------
fit_open <- glm(Open ~ ClaimSeverity, data=dfBinomial, family="binomial")
summary(fit_open)

## ------------------------------------------------------------------------
fit_open_1 <- glm(Open ~ 0 + ClaimSeverity, data=dfBinomial, family="binomial")
fit_open_2 <- glm(Open ~ 1 + ClaimSeverity, data=dfBinomial, family="binomial")
summary(fit_open_1)
summary(fit_open_2)

## ------------------------------------------------------------------------
fit_open_1 <- glm(Open ~ 0 + ClaimSeverity, data=dfBinomial, family=binomial(link="identity"))
fit_open_2 <- glm(Open ~ 1 + ClaimSeverity, data=dfBinomial, family=binomial(link="identity"))
summary(fit_open_1)
summary(fit_open_2)

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(ClaimSeverity, Open)) + geom_hex()
plt <- plt + scale_x_continuous(labels=scales::dollar)
plt + geom_smooth(method = "glm", formula = y ~ 0 + x, method.args = list(family = "binomial"))

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(OpenProb, Open)) + geom_hex()
plt + geom_smooth(method = "glm", formula = y ~ 1 + x, method.args = list(family = "binomial"))


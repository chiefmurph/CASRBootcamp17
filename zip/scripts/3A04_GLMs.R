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
load('./slides/data/glm.rda')

## ----fig.height=4--------------------------------------------------------
plt <- ggplot(dfGLM, aes(NumClaims)) + geom_histogram(binwidth = 1)
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(Payroll, NumClaims)) + geom_hex()
plt <- plt + scale_x_continuous(labels = scales::dollar) + ylab("# of Claims")
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfGLM, aes(Payroll, NumClaims)) + geom_hex()
plt <- plt + scale_x_continuous(labels = scales::dollar) + ylab("# of Claims")
plt + geom_smooth(method = "lm", formula = y ~ 1 + x)

## ----echo=TRUE-----------------------------------------------------------
fit_lm <- lm(NumClaims ~ Payroll, data = dfGLM)

## ----echo=TRUE-----------------------------------------------------------
fit_glm <- glm(NumClaims ~ Payroll, data = dfGLM, family = "poisson")

## ----echo=TRUE, collapse=TRUE--------------------------------------------
fit_1 <- glm(NumClaims ~ 1 + Payroll, data = dfGLM, family="poisson")

fit_2 <- glm(NumClaims ~ 1, data = dfGLM, family="poisson", offset=log(Payroll))

fit_1$aic
fit_2$aic
coef(fit_1)
coef(fit_2)

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(ClaimSeverity, Open)) + geom_hex()
plt <- plt + scale_x_continuous(labels=scales::dollar)
plt

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(ClaimSeverity, Open)) + geom_hex()
plt <- plt + scale_x_continuous(labels=scales::dollar)
plt + geom_smooth(method = "glm", formula = y ~ 0 + x, method.args = list(family = "binomial"))

## ------------------------------------------------------------------------
plt <- ggplot(dfBinomial, aes(OpenProb, Open)) + geom_hex()
plt + geom_smooth(method = "glm", formula = y ~ 1 + x, method.args = list(family = "binomial"))

## ------------------------------------------------------------------------
fit_open_1 <- glm(Open ~ 0 + ClaimSeverity, data=dfBinomial, family="binomial")
fit_open_2 <- glm(Open ~ 1 + ClaimSeverity, data=dfBinomial, family="binomial")
summary(fit_open_1)
summary(fit_open_2)
# dfBinomial$Predicted <- predict(fit_closure, type = "response")

## ------------------------------------------------------------------------
fit_open_1 <- glm(Open ~ 0 + ClaimSeverity, data=dfBinomial, family=binomial(link="identity"))
fit_open_2 <- glm(Open ~ 1 + ClaimSeverity, data=dfBinomial, family=binomial(link="identity"))
summary(fit_open_1)
summary(fit_open_2)


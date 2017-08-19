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



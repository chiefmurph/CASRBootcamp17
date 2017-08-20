## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
knitr::opts_chunk$set(
    warning=FALSE
  , error=FALSE
  , echo=TRUE
  , message=FALSE
  , fig.height = 4.5
  , fig.pos="t"
  , collapse = TRUE
)
knitr::opts_knit$set(root.dir = normalizePath('../../'))

## ------------------------------------------------------------------------
library(raw)
data(RegionExperience)
library(ggplot2)

basePlot <- ggplot(RegionExperience)
class(basePlot)

## ------------------------------------------------------------------------
basePlot <- basePlot + aes(x = PolicyYear, y = NumClaims, color=Region)

## ------------------------------------------------------------------------
p <- basePlot + geom_line()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_point()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_point() + geom_line()
p

## ------------------------------------------------------------------------
p <- ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims, group=Region, color=Region)) + geom_line()
p

## ------------------------------------------------------------------------
p <- basePlot + geom_bar(stat="identity", aes(fill = Region))
p

## ------------------------------------------------------------------------
p <- basePlot + geom_bar(stat="identity", position="dodge", aes(fill=Region))
p

## ------------------------------------------------------------------------
data(StateExperience)
p <- ggplot(StateExperience, aes(x = PolicyYear, y = NumClaims, color = State)) + geom_point() + facet_wrap(~ Region)
p <- p + theme(legend.position = "none")
p

## ------------------------------------------------------------------------
p <- ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims, group=Region, color=Region)) + geom_point()
p + geom_smooth(se = FALSE)

## ------------------------------------------------------------------------
p + geom_smooth(method = lm)

## ------------------------------------------------------------------------
p + scale_y_continuous(labels = scales::comma)

## ------------------------------------------------------------------------
library(raw)
data("RegionExperience")
plt1 <- ggplot(RegionExperience, aes(x = PolicyYear, y = NumClaims)) + geom_point()
plt1

plt2 <- plt1 + aes(color = Region)
plt2

plt3 <- plt2 + stat_smooth(method = "lm")
plt3

RegionExperience$Frequency <- with(RegionExperience, NumClaims / NumPolicies)

plt4 <- ggplot(RegionExperience, aes(x = PolicyYear, y = Frequency, color = Region)) + geom_point() + geom_line() + stat_smooth(method = lm)
plt4

## ------------------------------------------------------------------------
data("StateExperience")
pltExtra <- ggplot(StateExperience, aes(x = PolicyYear, y = NumClaims, color = Postal)) + geom_point() + geom_line()
pltExtra + facet_wrap(~ Region)


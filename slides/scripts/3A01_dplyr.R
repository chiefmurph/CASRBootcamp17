## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(
    warning=FALSE
  , error=FALSE
  , echo=TRUE
  , message=FALSE
)
knitr::opts_knit$set(root.dir = normalizePath('../../'))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

## ------------------------------------------------------------------------
library(Lahman)

## ----echo=TRUE, eval=FALSE-----------------------------------------------
## library(Lahman)
## data(package = 'Lahman')

## ------------------------------------------------------------------------
library(tidyr)
library(dplyr)

## ------------------------------------------------------------------------
dfSalaryByYear <- Salaries %>% 
  group_by(yearID) %>% 
  summarise(AvgSalary = mean(salary)
            , MedianSalary = median(salary))

## ------------------------------------------------------------------------
dfPlayerSalary <- Master %>% 
  filter(is.na(deathDate)) %>% 
  select(playerID, bats) %>% 
  inner_join(Salaries)

dfSalarySummary <- dfPlayerSalary %>% 
  summarise(MinSalary = min(salary)
            , MaxSalary = max(salary)
            , AvgSalary = mean(salary))

## ------------------------------------------------------------------------
dfYearAndHand <- dfPlayerSalary %>% 
  filter(bats != 'B') %>% 
  group_by(yearID, bats) %>% 
  summarise(AvgSalary = mean(salary)) %>% 
  spread(bats, AvgSalary) %>% 
  mutate(LeftRelativity = L / R)


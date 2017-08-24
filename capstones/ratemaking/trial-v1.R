#' ---
#' title:  "Ratemaking Capstone v1"
#' author: "ALR"
#' date:   "August 21, 2017"
#' output: html_document
#' ---



#+ include=FALSE
# knitr::opts_chunk$set(echo = FALSE)
require(alrtools)
require(magrittr)
require(dplyr)
require(tidyr)
require(ChainLadder)
require(tree)
require(randomForest)
source('resources.R')
getwd()
dir('./share', pattern = 'RData')



#' Load training data
load("./share/clms_train.RData")
load("./share/pol1_train.RData")
load("./share/pol3_train.RData")
load("./share/tri_train.RData")



#' ## Look at `str` for all tables.
ls()
str(clms_train)
str(pol1_train)
str(pol3_train)
str(tri_train)



#' Steps to follow:
#'   1. Check that claims data balances
#'   1. Develop triangle to utimate
#'   1. Attempt to find claims trend
#'        a. In aggregate, by CM year, using all claims developed
#'        a. In aggregate, by CM year, using closed claims only
#'   1. Trend closed claims
#'   1. Attempt to fit severity curve to trended closed claims
#'   1. Merge two data tables, spread
#'   1. Summarize claim counts by policy
#'   1. Add claim counts to policy table
#'   1. TODO



#' ## Create one policy table
pol3_wide <- pol3_train %>% 
  spread(variable, value) %>% 
  mutate(
    revenue = as.numeric(revenue),
    year_started = as.numeric(year_started),
    employee_count = as.numeric(employee_count),
    five_year_claims = as.numeric(five_year_claims)
  )

pol1_train <- pol1_train %>% 
  mutate(inception = as.numeric(inception)) %>% 
  mutate(expiration = as.numeric(expiration))

pols <- pol1_train %>% 
  inner_join(pol3_wide, by = 'policy_number')




#' ## What is the exposure by year?
#' Sum up revenue, policy months, both ear
pols$duration <- diff_yyyymm(
  pols$inception,
  pols$expiration
)
 
pols$exposed_months <- diff_yyyymm(
  pmin(pols$inception, 201701),
  pmin(pols$expiration, 201701)
) 

pols$exposed_revenue <- 
  pols$revenue / 12 * pols$exposed_months

pols$exposed_emp_count <-
  pols$employee_count / 12 * pols$exposed_months

pols$py <- year_yyyymm(pols$inception)

pols_py <- pols %>% 
  group_by(py) %>% 
  summarize(
    policy_count = n(),
    duration = sum(duration),
    revenue = sum(revenue),
    employee_count = sum(employee_count),
    exposed_months = sum(exposed_months),
    exposed_revenue = sum(exposed_revenue),
    exposed_emp_count = sum(exposed_emp_count)
  ) %>% 
  mutate(exposed_factor = exposed_months / duration) %>% 
  mutate(exposed_count =exposed_factor * policy_count)

  


#' ## Claims Balancing
#' Is the current incurred equal to the most recent diagonal?
sum(clms_train$claim_at_val)
sum(diag(as.matrix(tri_train[, 11:2])))




#' ## Develop triangle to ultimate
#' First create an object `tri` that is a triangle.
#' This requires converting to a matrix and then unnaming,
#' while removing the first column.
tri <- tri_train[, 2:11] %>% as.matrix %>% unname %>% as.triangle
tri
dim(tri)



#' ## Mack Method
#' Use the default as there is no development after `dev = 6`.
mm <- MackChainLadder(tri)
summary(mm)

#' The total ultimate is 
mm$FullTriangle[, 10] %>% sum

#' Get the LDFs for developing claims by year for severity calc
cumprod(mm$f[10:1])
ldfs_mm <- 1 / summary(mm)$ByOrigin$Dev.To.Date
ldfs_mm

pols$ldf <- ldfs_mm[pols$py - 2006]



#' ## Join claims and policy table
#' Also add report year
clms <- clms_train %>% 
  inner_join(pols, by = 'policy_number')

clms <- clms %>% 
  mutate(
    ry = year_yyyymm(clms$claim_made),
    count = 1,
    closed_count = ifelse(status == 'C', 1, 0),
    open_count = ifelse(status == 'O', 1, 0)
  )




#' ## Aggregate Claims by CM Year and Develop
clms_ry <- clms %>% 
  mutate(ultimate = claim_at_val * ldf) %>% 
  group_by(ry) %>% 
  summarize(
    count = n(), 
    incurred = sum(claim_at_val),
    closed_count = sum(closed_count),
    open_count = sum(open_count),
    ultimate = sum(ultimate)
  ) %>% 
  mutate(
    avg_sev_closed = incurred / closed_count,
    avg_sev_ult = ultimate / count
  )



#' ## Calculate trend 
a <- lm(
  log(avg_sev_ult) ~ ry,
  data = clms_ry,
  weights = clms_ry$closed_count
)
t1 <- exp(a$coefficients[2]) - 1

a <- lm(
  log(avg_sev_ult) ~ ry,
  data = clms_ry,
  weights = clms_ry$incurred / clms_ry$ultimate
)
t2 <- exp(a$coefficients[2]) - 1

trend <- (t1 + t2) / 2





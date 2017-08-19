# Author: Adam L. Rich
# Date:   August 16, 2017
# Description:
#
#   R script to create data needed for ratemaking capstone
#


require(devtools)
require(magrittr)
require(rmarkdown)
require(knitr)
require(alrtools)
require(MASS)


#############################################
# CLAIM SEVERITY
#   Average severity in 2016 will be $95k
#   Expect claims inflation of 3%
#   Model ultimate values using lognormal
#############################################

years              <- 2007:2016
inflation          <- 0.03
n_years            <- length(years)
inflation_factors  <- (1 + inflation) ^ (-9:0)
avg_severity       <- inflation_factors * 95000


# lognormal mean
#
#   E[X] = exp(mu + sigma^2/2)
#   log(E[x]) = mu + sigma^2/2
#   mu = log(E[x]) - sigma^2/2
#

sigma <- seq(from = 0.5, to = 5.0, by = 0.1)
mu <- log(95000) - sigma^2/2
sqrt((log(95000) - 9.0) * 2)



# Pick sigma as 2.2 and keep constant for each year
sigma <- rep(2.2, 10)
mu <- log(avg_severity) - sigma^2/2



#############################################
# POLICIES
#   Line is A&E
#   Exposure base is revenue
#
#   Frequency of claim is a function of
#     Region
#     Revenue
#     Primary Discipline
# 
#   Also include
#     Number of Architects
#     Whether they use written contracts
#     Longevity in business
#     Number of Claims in Last Five Years
#
#############################################



# Number of policies
n <- 1e5
policies <- data.frame(index = 1:n)



set.seed(912387)

policies$policy_number <- paste0(
  'C1AE', right(paste0('00000000', sample(1:1e6, n)), 8))

policies$policy_year <- sample(
  x = 2007:2016, 
  size = n, 
  prob = c(1, 2, 3, 4, 5, 5, 5, 5, 5, 5), 
  replace = TRUE
)

policies$duration_months <- sample(
  x = c(6:18), 
  size = n, 
  prob = c(2, 1, 1, 1, 1, 5, 100, 10, 5, 1, 1, 1, 20), 
  replace = TRUE
)

policies$policy_month <- sample(1:12, n, replace = TRUE)

policies$inception <- paste0(
  policies$policy_year, 
  right(paste0('00', policies$policy_month), 2)
)

a <- (policies$policy_month + policies$duration_months)
table(a)
b <- floor((a - 1) / 12)
table(b)

policies$expiration <- paste0(
  policies$policy_year + b, 
  right(paste0('00', a - 12*b), 2)
)

# Revenue
#   Minimum revenue should be 50e3
#   Maximum should be 5e6

revenue_low   <- c(50e3, 100e3, 250e3, 500e3, 1e6, 2.5e6)
revenue_high  <- c(revenue_low[-1], 5e6)
n_rev         <- 6

policies$revenue_bucket <- sample(
  x = 1:6,
  size = n,
  prob = c(6, 5, 4, 3, 2, 1),
  replace = TRUE
)

a <- runif(n)
b <- policies$revenue_bucket
policies$revenue <- (revenue_high[b] - revenue_low[b]) * a + 
  revenue_low[b]

# State/Region
#   Use CSV table for groupings
states <- read.csv('states.csv', stringsAsFactors = FALSE)

a <- sample(1:nrow(states), n, prob = states$Population, replace = TRUE)

policies$state <- states$State[a]
policies$state_group <- states$Frequency.Group[a]

a <- factor(policies$state_group, levels = c('Low', 'Mid', 'High'))

policies$state_relativity <- c(1.0, 1.5, 2.0)[as.integer(a)]


# Disciplines
#  Use CSV table for relativities
disciplines <- read.csv('disciplines.csv', stringsAsFactors = FALSE)
a <- sample(
  x = 1:nrow(disciplines), 
  size = n, 
  prob = disciplines$Probability, 
  replace = TRUE
)

policies$discipline <- disciplines$Discipline[a]
policies$discipline_relativity <- disciplines$Relativity[a]
policies$discipline_group <- paste0('d', policies$discipline_relativity)


# Revenue frequency
#   Expected is 0.05 per million in claims

policies$revenue_frequency <- policies$revenue / 1e6 * 0.05
policies$expected_frequency <- 
  policies$revenue_frequency * policies$discipline_relativity *
  policies$state_relativity

odf <- 2.5

policies$claim_count <- rnegbin(
  n = n, 
  mu = policies$expected_frequency, 
  theta = policies$expected_frequency / 1.5
)



#############################################
# GLM TEST for FREQ
#
#############################################

glm_freq <- glm(
  data = policies,
  formula = claim_count ~ 
    state_group + discipline_group + revenue + duration_months,
  family = quasipoisson
)

summary(glm_freq)





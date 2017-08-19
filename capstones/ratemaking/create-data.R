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
source('resources.R')



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

policies$state_relativity <- c(1.0, 1.25, 1.5)[as.integer(a)]

# If revenue is over $4m state does not matter
policies$state_relativity[policies$revenue >= 4e6] <- 2.0




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
#   Remember to adjust rev as it is annual!

policies$revenue_frequency <- policies$revenue / 1e6 * 
  0.05 * policies$duration_months / 12

policies$expected_frequency <- 
  policies$revenue_frequency * policies$discipline_relativity *
  policies$state_relativity


# Get expected claim counts
odf <- 2.5

policies$claim_count <- rnegbin(
  n = n, 
  mu = policies$expected_frequency, 
  theta = policies$expected_frequency / (odf - 1)
)




#############################################
# GLM TEST for FREQ
# TODO Need to flesh this out
#############################################

glm_freq <- glm(
  data = policies,
  formula = claim_count ~ 
    state_group + discipline_group + revenue + duration_months,
  family = quasipoisson
)

summary(glm_freq)





#############################################
# CLAIMS
#############################################

# Number of claims total
m <- sum(policies$claim_count)
claims <- data.frame(claimindex = 1:m)

# Merge with policies table
lpolicies <- policies[policies$claim_count > 0, ]
a <- cumsum(lpolicies$claim_count)
b <- c(1, a[-length(a)] + 1)
lpolicies$claimindex_start <- b
claims <- cbind(claims, lookup(claims, lpolicies))



# Claim made date
#   Select a month at random from the duration
a <- floor(runif(m) * (claims$duration_months + 3))
claims$claim_made <- add_mo(claims$inception, a)

# Claim closed date
#   Expect duration to be 2 years from report
#   Most between 1 and 3, so normal with s.d. = 0.5, mu = 2
a <- round(rnorm(m, mean = 24, sd = 6), 0)
a[a < 0] <- 0
claims$claim_closed <- add_mo(claims$claim_made, a)

# Claim status
# Valuation date is 201706
claims$status <- ifelse(claims$claim_closed <= 201706, 'C', 'O')


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

lparam <- data.frame(
  policy_year = years,
  sev_mu = mu,
  sev_sigma = sigma
)

# Ultimate claim value
a <- lookup(claims, lparam)
claims$sev_mu <- a$sev_mu
claims$sev_sigma <- a$sev_sigma

claims$claim_ultimate <- rlnorm(m, claims$sev_mu, claims$sev_sigma)



# Figure out claim values at each dev point
claims <- within(claims, {
  d000 <- 0
  d012 <- claims$claim_ultimate
  d024 <- claims$claim_ultimate
  d036 <- claims$claim_ultimate
  d048 <- claims$claim_ultimate
  d060 <- claims$claim_ultimate
  d072 <- claims$claim_ultimate
  d084 <- claims$claim_ultimate
  d096 <- claims$claim_ultimate
  d108 <- claims$claim_ultimate
  d120 <- claims$claim_ultimate
})

claims$age_at_close <- 
  diff_mo(claims$policy_year * 100, claims$claim_closed)

claims$age_at_open <- 
  diff_mo(claims$policy_year * 100, claims$claim_made)

claims$age_at_val <- 
  diff_mo(claims$policy_year * 100, 201612)

for(i in 1:10) {
  
  a <- runif(m)
  val <- add_mo(200700, i * 12)
  val_prior <- add_mo(val, -12)
  d <- paste0('d', right(paste0('000', i * 12), 3))
  d_prior <- paste0('d', right(paste0('000', (i - 1) * 12), 3))
  inc_prior <- claims[, d_prior]
  
  claims[, d] <- NA
  
  # If not yet open, set to 0
  l <- claims$age_at_open > (i * 12)
  claims[l, d] <- 0
  
  # If closed, set to ultimate
  l <- claims$age_at_close <= (i * 12)
  claims[l, d] <- claims$claim_ultimate[l]
  
  # If runif > 0.7, leave at last
  l <- (a > 0.7) & (is.na(claims[, d]))
  claims[l, d] <- claims[l, d_prior]
  
  # Otherwise interpolate to Ultimate
  l <- (a <= 0.7) & (is.na(claims[, d]))
  claims[l, d] <- (claims$claim_ultimate[l] - claims[l, d_prior]) * 
    a[l] / 0.7 + claims[l, d_prior]
  
}

claims$claim_at_val[claims$policy_year == 2007] <- claims$d120[claims$policy_year == 2007]
claims$claim_at_val[claims$policy_year == 2008] <- claims$d108[claims$policy_year == 2008]
claims$claim_at_val[claims$policy_year == 2009] <- claims$d096[claims$policy_year == 2009]
claims$claim_at_val[claims$policy_year == 2010] <- claims$d084[claims$policy_year == 2010]
claims$claim_at_val[claims$policy_year == 2011] <- claims$d072[claims$policy_year == 2011]
claims$claim_at_val[claims$policy_year == 2012] <- claims$d060[claims$policy_year == 2012]
claims$claim_at_val[claims$policy_year == 2013] <- claims$d048[claims$policy_year == 2013]
claims$claim_at_val[claims$policy_year == 2014] <- claims$d036[claims$policy_year == 2014]
claims$claim_at_val[claims$policy_year == 2015] <- claims$d024[claims$policy_year == 2015]
claims$claim_at_val[claims$policy_year == 2016] <- claims$d012[claims$policy_year == 2016]


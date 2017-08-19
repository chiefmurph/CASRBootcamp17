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


# Line is A&E, exposure base is revenue


# Frequency of claim is a function of
#
#   State
#   Revenue
#   Primary Discipline
#   Number of Claims in Last Five Years
# 

# Number of policies
n <- 1000

set.seed(912387)

policies <- data.frame(
  index = 1:n,
  policy_number = paste0('C1AE', right(paste0('00000000', sample(1:1e6, n)), 8)),
  policy_year = sample(2007:2016, n, prob = c(1, 2, 3, 4, 5, 5, 5, 5, 5, 5), replace = TRUE),
  duration_months = sample(c(6:18), n, prob = c(2, 1, 1, 1, 1, 5, 100, 10, 5, 1, 1, 1, 20), replace = TRUE)
)









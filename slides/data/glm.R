set.seed(1234)

num_policies <- 5e3

payroll <- runif(num_policies, 500e3, 10e6)
years_in_operation <- rpois(num_policies, 5)
number_of_employees <- as.integer(runif(num_policies, 50, 2e3))
claims_per_mil <- 1.5 - .1 * years_in_operation + 0.001 * number_of_employees
lambda <- payroll / 1e6 * claims_per_mil
num_claims <- rpois(num_policies, lambda)

dfGLM <- data.frame(
    NumClaims = num_claims
  , Payroll = payroll
  , YearsInOperation = years_in_operation
  , NuberOfEmployees = number_of_employees)

total_claims <- sum(num_claims)
claim_severity <- rgamma(total_claims, 10, 1/10e3)
open_prob <- pmin(claim_severity / 500e3, 1)

dfBinomial <- data.frame(
      ClaimSeverity = claim_severity
    , OpenProb = open_prob
    , Open = rbinom(total_claims, 1, open_prob)
  )

save(
    file = "./data/glm.rda"
  , dfGLM
  , num_policies
  , dfBinomial
  , total_claims
)

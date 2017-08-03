set.seed(1234)

num_policies <- 5000
claims_per_mil <- 1.5
claim_inflation <- .04

payroll <- runif(num_policies, 500000, 10000000)
lambda <- payroll / 1000000 * claims_per_mil
num_claims <- rpois(num_policies, lambda)

dfGLM <- data.frame(Payroll = payroll, NumClaims = num_claims)

total_claims <- sum(num_claims)
claim_severity <- rgamma(total_claims, 10, 1/10000)
open_prob <- claim_severity / 500000

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

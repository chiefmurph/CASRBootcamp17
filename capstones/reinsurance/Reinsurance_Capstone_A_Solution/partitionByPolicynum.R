# partitionByPolicynum.r
partitionByPolicynum <- function(policytbl, claimtbl) {
  # policytbl = policytbl
  # claimtbl = claimtbl
  # We want to divide policytbl into three sets A, B, C where all the policies of each policyholder
  #   are in the same set
  # We know the policyholder is uniquely identified by the field 'policynum'
  # and the policynum field is the combination of state abbreviation and
  # a five digit integer.
  # We assume the last digit of the integer is uniformly distributed.
  
  # One method: Count the number of policyholders, divide that number into thirds,
  # and use that to map to A, B, C. Then partition claimtbl accordingly.
  upn <- unique(policytbl$policynum)
  N <- length(upn)
  nA <- nB <- floor(N/3)
  NC <- N - nA - nB
  
  # Scramble the policy numbers
  upn <- upn[sample(1:N, replace = FALSE)]
  
  # Divide the policy numbers into three character vectors
  pnA <- upn[1:nA]
  pnB <- upn[(nA + 1): (nA + nB)]
  pnC <- upn[(nA + nB + 1): N]
  
  # split the policy table into three tables
  pA <- policytbl[policytbl$policynum %in% pnA, ]
  pB <- policytbl[policytbl$policynum %in% pnB, ]
  pC <- policytbl[policytbl$policynum %in% pnC, ]
  
  # split the claim table accordingly
  cA <- claimtbl[claimtbl$policykey %in% pA$policykey, ]
  cB <- claimtbl[claimtbl$policykey %in% pB$policykey, ]
  cC <- claimtbl[claimtbl$policykey %in% pC$policykey, ]
  
  # return a list holding each list of pairs
  list(
    test = list(policytbl = pA, claimtbl = cA),
    train = list(policytbl = pB, claimtbl = cB),
    validate = list(policytbl = pC, claimtbl = cC)
  )
}
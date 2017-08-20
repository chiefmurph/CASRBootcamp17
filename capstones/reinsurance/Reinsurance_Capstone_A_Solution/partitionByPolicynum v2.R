# partitionByPolicynum v2.r
partitionByPolicynum <- function(policytbl, claimtbl) {
  # p = policytbl
  # c = claimtbl
  # We want to divide p into three sets A, B, C where all the policies of each policyholder
  #   are in the same set
  # We know the policyholder is uniquely identified by the field 'policynum'.
  # Cut up the unique policynum's into three groups using new cut.character method
  source("cut.character.R")
  upn <- unique(policytbl$policynum)
  # Scramble them up
  upn <- upn[sample(1:length(upn), replace = FALSE)]
  # Here's the cut
  fupn <- cut(upn, breaks = 3)
  # Give names for policynumber lookup (next)
  names(fupn) <- upn
  # Lookup the group corresponding to the row's policynum field
  policyGroup <- fupn[policytbl$policynum]
  # Split the policy table into three groups according to the policynum's group
  S <- split(policytbl, policyGroup)
  # Split up claimtbl accordingly.  
  C1 <- claimtbl[claimtbl$policykey %in% S[[1]]$policykey, ]
  C2 <- claimtbl[claimtbl$policykey %in% S[[2]]$policykey, ]
  C3 <- claimtbl[claimtbl$policykey %in% S[[3]]$policykey, ]
  # Do we have all the claim on one grouup or another?
  stopifnot(nrow(claimtbl) == nrow(C1) + nrow(C2) + nrow(C3))
  # Return three (policytbl, claimtbl) pairs
  list(
    test = list(
      policytbl = S[[1]],
      claimtbl = C1
    ),
    train = list(
      policytbl = S[[2]],
      claimtbl = C2
    ),
    validate = list(
      policytbl = S[[3]],
      claimtbl = C3
    )
  )
}

# A Reinsurance Pricing Formula
# Foundations, 3rd ed., p. 295
# reinsurancePricingFormula.R
reinsurancePricingFormula <- function(RLC, CR, BF, IXL, TER) {
  # RLC = reinsurer's expected aggregate loss
  # CR = reinsurance ceding commission rate
  # BF = reinsurance brokerage fee (if any)
  # IXL = reinsurer's internal expense loading 
  #       as a percent of premium net of CR and BF
  # TER = reinsurer's target economic return
  #       as a percent of pure risk premium, net of CR, BR, and IXL
  RLC / (1 - CR - BF) / (1 - IXL) / (1 - TER)
}
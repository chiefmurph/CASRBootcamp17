# Burning Cost
# burningCost.R
burningCostXOL.rptd <- function(.data, limitLow, limitHigh = Inf, group_by = NULL) {
  .data <- .data %>% mutate(x = pmin(limitLow, rptd), y = pmin(limitHigh, rptd))
  if (is.null(group_by)) {
    p <- .data %>% 
      mutate(limitLow = pmin(limitLow, rptd), limitHigh = pmin(limitHigh, rptd)) %>%
      summarise(limitLow = sum(limitLow), limitHigh = sum(limitHigh))
  }
  else {
    p <- .data %>% 
      mutate(limitLow = pmin(limitLow, rptd), limitHigh = pmin(limitHigh, rptd)) %>%
      group_by_(group_by) %>%
      summarise(limitLow = sum(limitLow), limitHigh = sum(limitHigh)) %>%
      arrange_(group_by)
  }
  p <- p %>% mutate(burningCost = limitHigh - limitLow)
  bc <- p$burningCost
  if (!is.null(group_by)) names(bc) <- as.character(p[[1]])
  bc
}
burningCostASL.rptd <- function(.data, limitLowLR, limitHighLR, group_by = "py") {
  stopifnot(!is.null(group_by))
  p <- .data %>% 
      group_by_(group_by) %>%
      summarise(wp = sum(writtenpremium), rptd = sum(rptd),
                LR = rptd / wp,
                rptd.limitedLow = min(wp * limitLowLR, rptd),
                rptd.limitedHigh = min(wp * limitHighLR, rptd)) %>%
      arrange_(group_by)
  p <- p %>% mutate(burningCost = rptd.limitedHigh - rptd.limitedLow)
#  return(p)
  bc <- p$burningCost
  if (!is.null(group_by)) names(bc) <- as.character(p[[1]])
  bc
}

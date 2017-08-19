add_mo <- function(d, m) {
  d <- as.integer(d)
  m <- as.integer(m)
  dy <- floor(d / 100)
  dm <- d - (dy * 100)
  am <- dm + m
  ay <- ifelse(am == 0, 0, floor((am - 1) / 12))
  ry <- dy + ay
  rm <- am - ay * 12
  rd <- ry * 100 + rm
  return(rd)
}



diff_mo <- function(d1, d2) {
  yd1 <- floor(d1 / 100)
  yd2 <- floor(d2 / 100)
  md1 <- d1 - yd1 * 100
  md2 <- d2 - yd2 * 100
  md2 - md1 + (yd2 - yd1) * 12
}
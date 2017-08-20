# cut.character.r
cut.character <- function(x, breaks, labels = LETTERS[1:breaks], ...) {
  stopifnot(length(breaks) == 1L)
  cut(seq_along(x), breaks = breaks,labels = labels,  ...)
}
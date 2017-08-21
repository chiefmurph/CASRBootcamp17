## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
xout <- data.frame(
  claimno = 1:5,                              # integer
  value = rlnorm(5),                          # numeric
  date = as.Date("2017-01-01"),               # Date
  state = LETTERS[1:5],                       # character
  openclosed = c(rep(TRUE, 3), rep(FALSE, 2)) # logical
)
# What are the classes of each of the columns?
(classout <- sapply(xout, class))
write.csv(xout, "fiveclaims.csv", row.names = FALSE)

## ------------------------------------------------------------------------
# Read the file you just created
xin <- read.csv("fiveclaims.csv")
# What are the classes of each of the columns?
(classin <- sapply(xin, class))
classin == classout

## ------------------------------------------------------------------------
# Read the file you just created
xin2 <- read.csv("fiveclaims.csv", colClasses = unname(classout))
# What are the classes of each of the columns?
(classin2 <- sapply(xin2, class))
classin2 == classout

## ------------------------------------------------------------------------
#xin3 <- read.csv("fiveclaimsExcel.csv", colClasses = unname(classout))
#(classin3 <- sapply(xin3, class))
# Not crazy
#View(xin3)
# Whoa!

## ------------------------------------------------------------------------
#library(readxl)
#claims5 <- read_excel("fiveclaims.xlsx")
#View(claims5)
#sapply(claims5, class)


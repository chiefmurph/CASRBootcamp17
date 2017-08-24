## ----PleaseDontPrintThis-------------------------------------------------
myPassword <- "YoullNeverGuessThis"

## ----include=FALSE-------------------------------------------------------
myPassword <- "VerySecret"

## ---- echo=FALSE---------------------------------------------------------
library(dplyr)
library(raw)
data(MultiTri)
dfSummary <- MultiTri %>% 
  group_by(AccidentYear) %>% 
  summarise(TotalEP = )


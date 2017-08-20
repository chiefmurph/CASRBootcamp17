## ----include=FALSE-------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
knitr::opts_chunk$set(
    warning=FALSE
  , error=FALSE
  , echo=TRUE
  , message=FALSE
  , fig.height = 4.5
  , collapse = TRUE
)
knitr::opts_knit$set(root.dir = normalizePath('../../'))

## ------------------------------------------------------------------------
library(raw)
data("MultiTri")

## ----eval=FALSE----------------------------------------------------------
## View(MultiTri)

## ------------------------------------------------------------------------
financials <- select(MultiTri, CumulativePaid, CumulativeIncurred, IBNR)
no_lag <- select(MultiTri, -Lag)

## ------------------------------------------------------------------------
years <- select(MultiTri, contains("year"))

## ------------------------------------------------------------------------
new_tri <- rename(MultiTri, DevelopmentLag = Lag)

## ------------------------------------------------------------------------
new_tri <- mutate(
  new_tri
  , PaidToIncurred = CumulativePaid / CumulativeIncurred
  , Upper = DevelopmentYear == 1997)

## ------------------------------------------------------------------------
new_tri <- mutate(MultiTri, Upper = DevelopmentYear == 1997)
new_tri <- select(new_tri, -DevelopmentYear)

## ------------------------------------------------------------------------
1 %>% exp()

## ------------------------------------------------------------------------
1 %>% exp() %>% log()

## ----eval=FALSE----------------------------------------------------------
## MultiTri %>%
##   arrange(AccidentYear)
## 
## MultiTri %>%
##   arrange(desc(IBNR))

## ------------------------------------------------------------------------
upper_tri <- MultiTri %>% 
  filter(DevelopmentYear <= 1997)

## ------------------------------------------------------------------------
upper_tri <- MultiTri %>% 
  filter(DevelopmentYear <= 1997
         , IBNR > 500)

## ------------------------------------------------------------------------
every_fifth <- MultiTri %>%
  slice(seq(from = 5, by = 5, to = nrow(MultiTri)))

## ------------------------------------------------------------------------
df_grouped <- MultiTri %>% 
  group_by(Company, AccidentYear)

## ------------------------------------------------------------------------
dfBigYear <- MultiTri %>% 
  group_by(AccidentYear) %>% 
  summarise(BiggestIBNR = max(IBNR))

## ------------------------------------------------------------------------
dfBigCase <- MultiTri %>% 
  mutate(PaidToIncurred = CumulativePaid / CumulativeIncurred) %>% 
  filter(PaidToIncurred < 0.4) %>% 
  group_by(Company) %>% 
  arrange(desc(PaidToIncurred)) %>% 
  slice(1) %>% 
  select(Company, AccidentYear)


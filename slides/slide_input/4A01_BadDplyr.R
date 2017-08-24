library(dplyr)
library(raw)
data("MultiTri")
dfStuff <- MultiTri %>% 
  summarise(TotalPremium <- sum(NetEP))

dfStuff <- MultiTri %>% 
  dplyr::select(Company)




















dfLosses <- MultiTri %>% 
  filter(Company == "New Jersey Manufacturers Grp"
         , AccidentYear + Lag - 1 <= 1997
         , Line == "Workers Comp") %>% 
  select(-Line, -Company) %>% 
  arrange(AccidentYear, DevelopmentYear) %>% 
  group_by(AccidentYear) %>% 
  mutate(PriorIncurred = dplyr::lag(CumulativeIncurred)
         , PriorPaid = dplyr::lag(CumulativePaid)
         , IncurredLDF = CumulativeIncurred / PriorIncurred
         , PaidLDF = CumulativePaid / PriorPaid)

plt <- ggplot(dfLosses, aes(Lag, CumulativePaid, color = AccidentYear, group = AccidentYear)) + geom_line()
plt

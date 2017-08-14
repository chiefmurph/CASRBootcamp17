library(tidyverse)
library(lubridate)
mean.py.wp <- function(policytbl) 
c(
policytbl %>%
  mutate(py = year(policyeffdate)) %>%
  group_by(py) %>% summarise(wp = sum(writtenpremium)) %>%
  select(wp) %>% summarise(mean(wp)) %>% unlist()
)

## ------------------------------------------------------------------------
paste(c('X1','X2','X3'),collapse='+')
paste0('Y~',paste(c('X1','X2','X3'),collapse='+'))
as.formula(paste('Y~',paste(c('X1','X2','X3'),collapse='+')))

## ------------------------------------------------------------------------
library(MASS)
library(dplyr,quietly=TRUE,warn.conflicts=FALSE)
data(Insurance)
myIns<-Insurance
head(myIns) %>% knitr::kable()

## ------------------------------------------------------------------------
myIns %>% group_by(District) %>% 
  summarize(Holders=sum(Holders),Claims=sum(Claims)) %>%
  mutate(Frequency=Claims/Holders) %>% knitr::kable()

## ------------------------------------------------------------------------
myIns$DistFactor<-as.factor(paste0("Dist",myIns$District))
head(myIns) %>% knitr::kable()

## ------------------------------------------------------------------------
poissonModel1<-glm(Claims~DistFactor,family=poisson(link='log'),
data=myIns,offset=log(Holders))
summary(poissonModel1)

## ------------------------------------------------------------------------
d1<-c('Dist1-3','Dist1-3','Dist1-3','Dist4')
myIns$DistFactor2<-as.factor(d1[myIns$District])
poissonModel2<-glm(Claims~DistFactor2,family=poisson(link='log'),
data=myIns,offset=log(Holders))
summary(poissonModel2)

## ------------------------------------------------------------------------
str(poissonModel2)

## ------------------------------------------------------------------------
poissonModel2$coefficients

## ------------------------------------------------------------------------
class(summary(poissonModel2))
summary(poissonModel2)$coefficients

## ------------------------------------------------------------------------
poissonModel1$aic

poissonModel2$aic


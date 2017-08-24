## ----eval=FALSE----------------------------------------------------------
## install.packages('tree')
## install.packages('randomForest')
## library(MASS)
## library(dplyr)
## library(ggplot2)
## library(raw)
## library(tree)
## library(randomForest)

## ------------------------------------------------------------------------
library(MASS)
library(dplyr)
library(ggplot2)
library(raw)
library(tree)
library(randomForest)
library(bindrcpp)
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
data(Boston)
head(Boston)
bos <- Boston %>% 
  dplyr::select(dis, rm, medv) %>% 
  mutate(medv_f = cut(medv, breaks = 0:5 * 10))

## ------------------------------------------------------------------------
plot(bos)

## ------------------------------------------------------------------------
plot(medv ~ rm, data = bos)
plot(medv ~ log(dis), data = bos)

## ------------------------------------------------------------------------
p <- ggplot(data = bos) +
  geom_point(mapping = aes(
    x = rm, 
    y = log(dis),
    color = medv
  )) +
  scale_color_gradientn(colours = c('yellow', 'green', 'blue', 'red'))

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
tbos <- tree(
  formula = medv ~ rm + log(dis),
  data = bos
)
summary(tbos)

## ------------------------------------------------------------------------
plot(tbos)

## ------------------------------------------------------------------------
plot(tbos)
text(tbos)

## ------------------------------------------------------------------------
plot(tbos, type = 'uniform')
text(tbos, pretty = 5, col = 'blue', cex = 0.8)

## ------------------------------------------------------------------------
data(RegionExperience)
RegionExperience %>% head

## ------------------------------------------------------------------------
plot(RegionExperience)
unique(RegionExperience$Region)
unique(RegionExperience$PolicyYear)

## ------------------------------------------------------------------------
rex <- RegionExperience %>% 
  mutate(Freq = NumClaims / NumPolicies) %>% 
  dplyr::select(Region, PolicyYear, Freq)
unique(rex$Region)

## ------------------------------------------------------------------------
trex <- tree(
  formula = Freq ~ .,
  data = rex
)

## ------------------------------------------------------------------------
plot(trex, type = 'uniform')
text(trex, pretty = 10, col = 'blue', cex = 0.8)

## ------------------------------------------------------------------------
bos <- Boston %>% 
  mutate(medv_pr = medv / rm) %>% 
  dplyr::select(age, dis, medv_pr) %>% 
  mutate(medv_prf = cut(medv_pr, breaks = 0:5 * 2))

## ------------------------------------------------------------------------
p <- ggplot(data = bos) +
  geom_point(mapping = aes(
    x = age, 
    y = dis,
    color = medv_pr
  )) +
  scale_color_gradientn(colours = c('yellow', 'green', 'blue', 'red'))

## ------------------------------------------------------------------------
p

## ------------------------------------------------------------------------
tbos <- tree(
  formula = medv_pr ~ age + log(dis),
  data = bos
)

## ------------------------------------------------------------------------
plot(tbos, type = 'uniform')
text(tbos, pretty = 5, col = 'blue', cex = 0.8)

## ------------------------------------------------------------------------
rf <- randomForest(
  medv ~ ., data = Boston, 
  ntree = 100,
  nodesize = 100,
  importance = TRUE,
  mtry = 3)
rf

## ------------------------------------------------------------------------
plot(rf)

## ------------------------------------------------------------------------
importance(rf)

## ------------------------------------------------------------------------
par(mfrow = c(1, 2))
varImpPlot(rf)


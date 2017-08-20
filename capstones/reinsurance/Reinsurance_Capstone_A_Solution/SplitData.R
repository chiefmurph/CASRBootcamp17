# Split the data

load(file = "ReinsuranceCapstone 10.RData")
source("partitionByPolicynum v2.R")
Data.Partitioned <- partitionByPolicynum(policytbl, claimtbl)

save(Data.Partitioned, file = "Data.Partitioned.RData")

#library(readr)
#write_csv(Data.Partitioned$train$policytbl, "train_policytbl.csv")
#write_csv(Data.Partitioned$train$claimtbl, "train_claimtbl.csv")
#write_csv(Data.Partitioned$test$policytbl, "test_policytbl.csv")
#write_csv(Data.Partitioned$test$claimtbl, "test_claimtbl.csv")
#write_csv(Data.Partitioned$validate$policytbl, "validate_policytbl.csv")
#write_csv(Data.Partitioned$validate$claimtbl, "validate_claimtbl.csv")


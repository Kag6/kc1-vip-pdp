source("config.R")
source("R/lib_helpers.R")
library(dplyr)

set.seed(CFG$seed)

df <- readRDS("data/processed/kc1_full.rds")

# Stratified split (class oranını koru)
idx_true  <- which(df$defects == "true")
idx_false <- which(df$defects == "false")

n_true_train  <- floor(length(idx_true)  * CFG$train_frac)
n_false_train <- floor(length(idx_false) * CFG$train_frac)

train_idx <- c(sample(idx_true,  n_true_train),
               sample(idx_false, n_false_train))

train <- df[train_idx, ]
test  <- df[-train_idx, ]

saveRDS(train, "data/processed/train.rds")
saveRDS(test,  "data/processed/test.rds")

cat("Split OK\n")
cat("Train balance:\n"); print(prop.table(table(train$defects)))
cat("Test  balance:\n"); print(prop.table(table(test$defects)))
